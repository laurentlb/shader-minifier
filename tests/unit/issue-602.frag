#version 430
out vec4 fragColor;
uniform int idx;

// Initializer lists are only valid in declarations: a variable initialized
// with one can't be inlined, even when it's used only once.
int global1[] = {1};
const int global2[] = {2};
float singleUse()
{
    int arr[] = {1};
    const int carr[] = {2};
    float farr[1] = {3.};
    return float(arr[0] + carr[0] + global1[0] + global2[0]) + farr[0];
}

// Initializer lists can't be turned into an assignment when reusing a variable.
float reuseInitializerList()
{
    int arr[] = {1,2};
    float r = float(arr[0] + arr[1]);
    int arr2[] = {3,4};
    r += float(arr2[0] + arr2[idx]);
    float arr3[2] = {5.,6.};
    r += arr3[idx];
    float arr4[2] = {7.,8.};
    return r + arr4[idx];
}

// Implicitly sized arrays get their size from their init, so they can't be
// reused for another array with the same declared type.
float reuseImplicitlySized()
{
    int arr[] = int[](1,2);
    float r = float(arr[0] + arr[idx]);
    int arr2[] = int[](3,4,5);
    r += float(arr2[0] + arr2[idx]);
    int[] arr3 = int[](1,2);
    r += float(arr3[0] + arr3[idx]);
    int[] arr4 = int[](3,4,5);
    r += float(arr4[0] + arr4[idx]);
    // Explicit sizes are fine.
    int arr5[2] = int[](1,2);
    r += float(arr5[0] + arr5[idx]);
    int arr6[2] = int[](3,4);
    return r + float(arr6[0] + arr6[idx]);
}

// The unused variable is removed, the other one can't be inlined.
float unusedInitializerList()
{
    int arr[] = {1,2};
    int arr2[] = {1,2};
    return float(arr2[0]);
}

void main()
{
    fragColor = vec4(singleUse(), reuseInitializerList(), reuseImplicitlySized(), unusedInitializerList());
}
