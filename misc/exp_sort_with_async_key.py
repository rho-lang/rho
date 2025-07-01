from asyncio import run
from random import randrange
from time import time


def sync_key(n):
    return n


async def async_key(n):
    return n


print(sorted([1, 2, 3], key=sync_key))
try:
    [1, 2, 3].sort(key=async_key)
except TypeError as err:
    print(err)


def run_async_key(n):
    return run(async_key(n))


print(sorted([1, 2, 3], key=run_async_key))


xs = [randrange(1 << 20) for _ in range(1 << 16)]
start = time()
xs.sort()
print("no key", time() - start)

xs = [randrange(1 << 20) for _ in range(1 << 16)]
start = time()
xs.sort(key=sync_key)
print("sync key", time() - start)

xs = [randrange(1 << 20) for _ in range(1 << 16)]
start = time()
xs.sort(key=run_async_key)
print("async key", time() - start)