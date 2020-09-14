# quicksort.awk --- Quicksort algorithm, with user-supplied
#                   comparison function
#
# Arnold Robbins, arnold@skeeve.com, Public Domain
# January 2009


# quicksort --- C.A.R. Hoare's quicksort algorithm. See Wikipedia
#               or almost any algorithms or computer science text.
#
# Adapted from K&R-II, page 110

function quicksort(data, left, right, less_than,    i, last)
{
    if (left >= right)  # do nothing if array contains fewer
        return          # than two elements

    quicksort_swap(data, left, int((left + right) / 2))
    last = left
    for (i = left + 1; i <= right; i++)
        if (@less_than(data[i], data[left]))
            quicksort_swap(data, ++last, i)
    quicksort_swap(data, left, last)
    quicksort(data, left, last - 1, less_than)
    quicksort(data, last + 1, right, less_than)
}

# quicksort_swap --- helper function for quicksort, should really be inline

function quicksort_swap(data, i, j,      temp)
{
    temp = data[i]
    data[i] = data[j]
    data[j] = temp
}
