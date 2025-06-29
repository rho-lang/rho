def main():
    x = "foo"

    def f1():
        print(x)

    def f2():
        nonlocal x
        x = "bar"

    f1()
    x = "baz"
    f1()
    f2()
    f1()


if __name__ == "__main__":
    main()
