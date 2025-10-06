## TASK 2

IndexOfMin <- function(array, first, last){
   index <- first 
   for (k in (first + 1):last){
     if (array[k] < array[index]){
       index <- k}}
   return (index)
}

" num of op. | repetitions
4 1 | 1
5 2*last + 2 | 1
6 1 | last
7 1 | last

T = 1 + (2*last+2) + last + last
"

print(IndexOfMin(c(4, 2, 1, 3, 5), 1,5))

## Task3

SelectionSort <- function(array, n){
  for (i in (1:(n-1))){
    j <- IndexOfMin(array, i, n)
    swap <- array[i]
    array[i] <- array[j]
    array[j] <- swap
  }
  return (array)
}

" num of op. | repetitions
25 2*(n-1)+2 | 1
26 1 + (2*last+2) + last + last | n-1
27 1 | n-1
28 1 | n-1
29 1 | n-1
"

print(SelectionSort(c(4, 2, 1, 3, 5), 5))

## TASK 4

RecursiveSelectionSort <- function(array, first, last){
  if (first < last){
    index <- IndexOfMin(array, first, last)
    swap <- array[first]
    array[first] <- array[index]
    array[index] <- swap
    array <- RecursiveSelectionSort(array, first + 1, last)}
  
  return (array)}

print(RecursiveSelectionSort(c(4, 3, 1, 2, 5), 1, 5))
