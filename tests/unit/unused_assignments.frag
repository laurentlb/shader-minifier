#version 330
int gi = 0;
void preventMergingConsecutiveAssignments() { gi++; }

void shouldRemoveAllAssignments() {
    float x = 1.;
    preventMergingConsecutiveAssignments();
    x = sin(x); // this assignment is eventually unused
    preventMergingConsecutiveAssignments();
    x = cos(x); // this assignment is unused
}
float shouldNotRemoveAnyAssignment() {
    float x = gi;
    preventMergingConsecutiveAssignments();
    x = sin(x); // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    x = cos(x); // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    return x;
}

vec3 bewareThatAssignmentToAFieldIsNotAFullOverwrite(vec3 v) {
    v.x = 1.; // this assignment is unused
    preventMergingConsecutiveAssignments();
    v = vec3(0.3); // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    v.y = 0.; // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    v.x += 1.; // this assignment is unused
    preventMergingConsecutiveAssignments();
    vec3 vv = v.yyz; // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    vv.x++; // this assignment is unused
    preventMergingConsecutiveAssignments();
    vv = vec3(0.5); // this assignment is NOT unused
    preventMergingConsecutiveAssignments();
    return vv;
}

void dontRemoveAssignmentsToOutParams(in int i, inout int io, out int o) {
    i = 4; // this assignment is unused
    o = 5; // this assignment is NOT unused
    io = 6; // this assignment is NOT unused
}

void dontRemoveUnusedAssignmentToGlobal() {
    gi = 8;
    gi = 9;
}

float removePureDeclInit() {
    vec3 i = vec3(0);
    preventMergingConsecutiveAssignments();
    i = vec3(1);
    vec3 j = vec3(gi++);
    preventMergingConsecutiveAssignments();
    j = vec3(1);
    return i.x * j.y;
}

int onlyRemove99AndAThenReturn15() {
    int a=1,b=99;
    a*=a+a+a;
    {
        int a = a;
        preventMergingConsecutiveAssignments();
        a = 2;
        b=a*a;
    }
    preventMergingConsecutiveAssignments();
    return a+a*b;
}

void main() {
	shouldRemoveAllAssignments();
	shouldNotRemoveAnyAssignment();
	bewareThatAssignmentToAFieldIsNotAFullOverwrite(vec4(1).xyz);
	int io = 2, o = 3; // o's assignment is unused!
	dontRemoveAssignmentsToOutParams(gi, io, o);
	dontRemoveUnusedAssignmentToGlobal();
	removePureDeclInit();
	onlyRemove99AndAThenReturn15();
}