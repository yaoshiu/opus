for n in range(1024):
    b = 1
    a = 1
    for i in range(n):
        t = a
        a = b
        b = t + b
    print(a)
