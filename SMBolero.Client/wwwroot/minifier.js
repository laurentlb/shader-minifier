const highlights = [];
const min2src = [];
const src2min = [];

function getHash(line, col, isMin) {
    return line * 1000 + col;
}

function extractAnnotations(str) {
    const lines = str.split('\n');
    for (let i = 0; i < lines.length; i++) {
        var hasReplaced = true;
        while (hasReplaced) {
            hasReplaced = false;
            lines[i] = lines[i].replace(/(\w+)@(-?\d+),(-?\d+)@/, function (match, ident, line, col, offset) {
                hasReplaced = true;
                line--;
                col--;
                min2src[getHash(i, offset)] = { ident, line, col };
                src2min[getHash(line, col)] = { ident, line: i, col: offset };
                return ident;
            });
        }
    }
    return lines.join('\n');
}

function getPrefix(isMin) {
    return isMin ? 'min' : 'src';
}

function addAnnotations(str, sourceMap, div, isMin) {
    const lines = str.split('\n');
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const lineDiv = document.createElement('div');
        lineDiv.classList.add('line');
        var html = line.replace(/\w+/g, function (match, offset) {
            // console.log(match, i, offset);
            const hash1 = getHash(i, offset);
            if (sourceMap[hash1]) {
                const hash2 = getHash(sourceMap[hash1].line, sourceMap[hash1].col);
                return `<a id=${getPrefix(isMin)}${hash1} class="ident" onclick="show(this, '${getPrefix(!isMin)}${hash2}')">${match}</a>`;
            }
            return match;
        });

        const comment = html.indexOf('//');
        if (comment !== -1) {
            html = html.substring(0, comment) + '<span class="comment">' + html.substring(comment) + '</span>';
        }

        lineDiv.innerHTML = html;
        div.appendChild(lineDiv);
    }
}

function show(x, hash) {
    // TODO: support multiple references
    const link = document.getElementById(hash);
    removeHighlights();
    addHighlight(link);
    addHighlight(x);
}
function removeHighlights() {
    for (const hl of highlights) {
        hl.classList.remove('highlight');
    }
    highlights.length = 0;
}


function addHighlight(item) {
    item.classList.add('highlight');
    item.scrollIntoView({ block: "center" });
    highlights.push(item);
}

function updateShader(shaderSource, shaderMinified) {
    const source = document.querySelector('.source');
    const minified = document.querySelector('.minified');

    removeHighlights();
    source.innerHTML = '';
    minified.innerHTML = '';
    min2src.length = 0;
    src2min.length = 0;

    const shaderMinified2 = extractAnnotations(shaderMinified);
    addAnnotations(shaderMinified2, min2src, minified, true);
    addAnnotations(shaderSource, src2min, source, false);
}

function copyBtn() {
    const minified = document.querySelector('.minified');
    console.log("Copy", minified.textContent);
    navigator.clipboard.writeText(minified.textContent);
}