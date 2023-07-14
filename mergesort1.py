import random
import time

def merge_sort(arr):
    """
    Merge Sort In Python

    Sorts an array using the merge sort algorithm.
    :param arr: The array to be sorted.
    :return: The sorted array.
    """
    if len(arr) <= 1:
        return arr
    mid = len(arr) // 2
    left = arr[:mid]
    right = arr[mid:]
    left = merge_sort(left)
    right = merge_sort(right)
    return merge(left, right)

def merge(left, right):
    """
    Merges two sorted arrays into a single sorted array.

    :param left: The left sorted array.
    :param right: The right sorted array.
    :return: The merged sorted array.
    """
    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1
    result += left[i:]
    result += right[j:]
    return result

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
    start_time = time.perf_counter()
    sorted_numbers = sort_fn(input)
    end_time = time.perf_counter()
    elapsed_time = end_time - start_time
    print(f"'Sorting {len(input)} {list_type} numbers took {elapsed_time:.3e} seconds.'")
    if sorted_numbers != sorted(input):
        print("'Error: list not sorted correctly!'")


random_numbers = generate_random_numbers(1000)
measure_execution_time(merge_sort, "random", random_numbers)

sorted_list = list(range(1, 1000001))
reversed_list = sorted_list[::-1]

measure_execution_time(merge_sort, "sorted", sorted_list)
measure_execution_time(merge_sort, "reversed", reversed_list)
