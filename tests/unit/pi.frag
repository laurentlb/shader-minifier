#version 400

out vec4 fragColor;

void main() {
    double pi = 3.141592653589793;
    double minus_pi = -3.141592653589793;
    double tau = 6.283185307179586;
    double minus_tau = -6.283185307179586;
    double half_pi = 1.5707963267948966;
    double minus_half_pi = -1.5707963267948966;
    double precise_pi = 3.14159265358979323846264338327950288419716939937510;
	fragColor = vec4(pi+minus_pi+tau+minus_tau+half_pi+minus_half_pi+precise_pi);
}
