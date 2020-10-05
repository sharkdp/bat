#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

void test_function()
{
#define TEST
#ifdef TEST
    printf("TEST is defined\n");
#endif
}

struct Node
{
    int val;
    struct Node *next;
};

int main(int argc, char **argv)
{
    /* This C program was written to help bat
     * with its syntax highlighting tests
     */

    // Calling test function
    test_function();
    struct Node *head = NULL;
    head = (struct Node *)malloc(sizeof(struct Node *));
    head->val = -1;
    if (head->val == 1 * -1)
    {
        head->val = 10;
    }
    else
    {
        head->val = argc;
    }
    int t = head->val, count = 0;
    free(head);
    while (t--)
    {
        count++;
    }
    for (int i = t; i < count; ++i)
    {
        do
        {
            --count;
        } while (false);
    }
    enum chars
    {
        M,
        I,
        T,
        H,
        L
    };
    char *string = "string";
    printf("true is #define true %d\n", true);
    return 0;
}
