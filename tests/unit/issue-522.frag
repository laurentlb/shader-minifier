#version 430
out vec4 fragColor;

float test(float[2] arr){
  float acc = 0.;
  for(int i = 0; i < 2; i++){
    acc += arr[i];
  }
  return acc;
}

void main() {
  // Initializer lists are only valid in declarations, don't inline them.
  float arr[2] = {1.,2.};
  fragColor.r = test(arr);
}
