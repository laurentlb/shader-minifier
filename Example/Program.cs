/**
 * Example of use of the ShaderMinifier library from C#.
 */

using ShaderMinifier;

var shader = """
out vec4 fragColor;
void main()
{
    fragColor = vec4(1., 1., 1., 1.);
}
""";

var file = new Tuple<string, string>("filename.frag", shader);
var options = Minifier.ParseOptions(new[] { "--format", "text" });
var minifier = new Minifier(options, new[] { file });
minifier.Format(System.Console.Out);
