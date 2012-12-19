# Lustre Interpreter

## Usage

Type command `make run` to run the code. The interpreter will interpret the lustre code in 'code.lus' in current directory and take data in 'in.data' as input.

### Input Format

The input file gives data in text format (ASCII). Each line represents input values in a basic clock, and each input value in a line is separated by whitespaces. There are four types of data: int, real, char, bool.

 *  **int:** written in number, like `12`, `23`.
 *  **real:** written in number that has a period, like `8.`, `7.6`.
 *  **char:** written in a character quoted by ', like `'a'`, `'f'`.
 *  **bool:** written as `t` to indicate true values or `f` to indicate false values.

The data in a line must appear in the same order as the corresponding arguments in the node declaration.

## Implementations

### Evaluating Directly

The simplest and most intuitive implementation. In this implemention precompiling is not required and the output values are directly evaluated through the expressions.

First, we find the output variables. To solve them, we search the equations for the values and get the expressions. Then we evaluate the expressions. In the process we might discover some variables in the expressions need to be computed, so we just solve the variables recursively. If the lustre program are written reasonably, the variables can be solved.

In this method we only care about the values we need. We use a 'context' variable to store the input variables and also the variables calculated. However, as the `pre` operator exists, we can't figure out the values of the past cycles are unnecessary. So we have to retain all the past values. They may be in a huge number. It's the main drawback.

### Compiling into Interconnected Nodes

If we see each operator as a node, the lustre program can be regarded as a large node consisting of many interconnected nodes, as illustrated in the lustre manual. There are two types of nodes, the stateful and the stateless. `->` and `pre` are stateful nodes and the others are stateless, that is, when we evaluate `->` and `pre`, we should take their states into account, output appropriate values, and update their states.

`->` is simple, its state only changes in the first cycle and then never changes. In the first cycle, it returns the left operand, and in the other cycles, it returns the right operand. All of them share the same state, so the state can be stored externally and globally. The state of `pre v` is the current value of v. Every cycle it returns the value stored in its state and take the current value as a new state. In this way, we do not need to store all the past values.

One thing to note is that we must evaluate all the `pre`s in the evaluation paths, no matter whether they have impacts on the final values because we can't analysis whether the state will be used in the next cycle (except that we only care about the right operands of `->`s because after the first cycle we will only evaluate the right operands).

There is also another small problem. Consider the following equation:

```lustre
s = 1 -> (pre s + 1)
```

If we evaluate the value recursively, when we evaluate `pre s`, we need to get the value of `s` to update the state. However, `pre s` hasn't been return now so we do not know what's the current value of `s`. A deadlock here. This should be treated specially. We need a two-pass evaluation: in the first evaluation values of variables are calculated; in the second one value states of `pre`s are calculated and updated.


## Problems and Solutions

### Clock Deduction

Think about the following equation:
```lustre
s = pre s
```
This may produce nil, nil, nil, ... , normally. A useless value.

When we evaluate an expression like `pre s`, what we do is to check the current clock of `s` (whether `s` is in its clock cycle) since `pre s` would return a value of the same clock as that of `s`. In this equation, `pre s` is assigned to `s`. So the current clock of `s` depends on itself. We could set it "on clock" or even "off clock". Weird? This may yield an error since no one will expect a variable of nil values, or we could also set it "on clock" as default to avoid the error.

Think about this:
```lustre
s = (1 when b) -> (pre s)
```

In this equation the current clock of `s` depends on itself as well. But we have more clues: because the clock `1 when b` is on `b`, so the clock of s is deduced to be on `b`. We have a clock deduction here.

#### Solution

We need a clock deduction on the time operators, and if the clock can't be deduced, it will be set as basic clock as default.

### List Evaluations

We would like our lustre interpreter to support assigning multiple variables at one time like this:
```lustre
(a, b) = (1 -> pre b + 1, a)
```
Intuitively, we would evaluate a list expression at one time. However, the list expression above depends on the left side. So list expressions can't be evaluated at one time.

#### Solution 1

Every time evaluating a expression, since it may yield a list, we need a number to select which element of the expression in order to assign it to the corresponding element in the left.

#### Solution 2

Precompiling expressions and splitting them into single assignments.
