def working_key(n):
    return -n

async def my_key(n):
    return -n

print(sorted([1, 2, 3], key=working_key))
# TypeError: '<' not supported between instances of 'coroutine' and 'coroutine'
[1, 2, 3].sort(key=my_key)