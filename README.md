A lustre interpreter.

## Problems and solutions

### Clock deduction

Think about the following equation:
```lustre
s = pre s
```
This produces 1, 1, 1, ... , normally.

When we evaluate an expression like `pre s`, what we do is to check the current clock of `s` (whether `s` is in its clock cycle) since `pre s` would return a value of the same clock as that of `s`. In this equation, `pre s` is assigned to `s`. So the current clock of `s` depends on itself. We could set it "on clock" or even "off clock". Weird? From the example above, we find out that we should set it "on clock" as default.

Think about this:
```lustre
s = (1 when b) -> (pre s)
```

In this equation the current clock of `s` depends on itself as well. But we have more clues: because the clock `1 when b` is on `b`, so the clock of s is deduced to be on `b`. We have a clock deduction here.

#### Conclusion

In a conclusion, we need a clock deduction on the time operators, and if the clock can't be deduced, it will be set as basic clock as default.

