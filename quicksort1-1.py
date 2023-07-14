import random
import time

def quicksort(arr):
    """
    Quick Sort In Python

    Sorts an array of integers using quicksort algorithm.
    :param arr: The array to be sorted.
    :return: The sorted array.
    """
    if len(arr) <= 1:
        return arr
    pivot = random.choice(arr)  # Select a random pivot element
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]
    return quicksort(left) + middle + quicksort(right)

def generate_random_numbers(n):
    """
    Generates a list of n random integers between 1 and 1000.

    :param n: The number of integers to generate.
    :return: The list of random integers.
    """
    return [random.randint(1, 1000) for _ in range(n)]

def measure_execution_time(sort_fn, list_type, input):
    """
    Measures the execution time of a sorting function.

    :param sort_fn: The sorting function to be measured.
    :param list_type: The type of list being sorted (e.g., "random", "sorted", "reversed").
    :param input: The list to be sorted.
    """
    start_time = time.time()
    sorted_numbers = sort_fn(input)
    end_time = time.time()
    elapsed_time = end_time - start_time
    print(f"'Sorting {len(input)} {list_type} numbers took {elapsed_time:.3e} seconds.'")
    if sorted_numbers != sorted(input):
        print("'Error: list not sorted correctly!'")

random_numbers = generate_random_numbers(1000)
measure_execution_time(quicksort, "random", random_numbers)

sorted_list = list(range(1, 1000001))
reversed_list = sorted_list[::-1]

measure_execution_time(quicksort, "sorted", sorted_list)
measure_execution_time(quicksort, "reversed", reversed_list)
