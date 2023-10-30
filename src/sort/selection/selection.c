#include <stdio.h>
#include <stdlib.h>

void swap(int *a, int *b)
{
    int temp = *a;
    *a = *b;
    *b = temp;
}

void selsort(int arr[], int len)
{
    for (int i = 0; i < len - 1; i++)
    {
        int min_idx = i;
        for (int j = i + 1; j < len; j++)
        {
            if (arr[j] < arr[min_idx])
            {
                min_idx = j;
            }
        }
        swap(&arr[min_idx], &arr[i]);
    }
}

int main()
{
    int size = 0;
    printf("Enter size of array:");
    scanf("%d", &size);

    int arr[size];
    for (int i = 0; i < size; i++)
    {
        arr[i] = rand();
    }

    selsort(arr, size);
    printf("Sorted array: \n");
    for (int i = 0; i < size; i++)
    {
        printf("%d ", arr[i]);
    }
    printf("\n");
    return 0;
}