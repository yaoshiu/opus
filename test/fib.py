def fib_iter(n, a, b):
    if n == 0:
        return a
    return fib_iter(n - 1, b, a + b)

def fib(n):
    return fib_iter(n, 1, 1)

print(fib(256))
