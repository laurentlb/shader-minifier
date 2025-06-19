struct MyStruct {
    float my_function() {
        return my_other_function() + 5;
    }
    
    float my_other_function() {
        return 10 + member_var;
    }

    int member_var;
};