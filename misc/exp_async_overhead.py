from time import time
from asyncio import run


def main():
    def f1():
        pass

    start = time()
    for _ in range(10_000_000):
        f1()
    print(time() - start)

    async def f2():
        async def f3():
            pass

        start = time()
        for _ in range(10_000_000):
            await f3()
        print(time() - start)

    run(f2())


if __name__ == "__main__":
    main()
