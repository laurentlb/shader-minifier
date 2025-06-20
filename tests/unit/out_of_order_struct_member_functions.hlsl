struct MyStruct {
    float my_function() {
        return my_other_function() + 5;
    }
    
    float my_other_function() {
        int something = 6;
        return 10 + member_var + something;
    }

    int member_var;
};

float my_other_function() {
    return 10 + member_var;
}