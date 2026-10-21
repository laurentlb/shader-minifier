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
        // Escape HTML special characters in the same pass, so that offsets still match the source.
        var html = line.replace(/\w+|[&<>]/g, function (match, offset) {
            if (match === '&') return '&amp;';
            if (match === '<') return '&lt;';
            if (match === '>') return '&gt;';
            const hash1 = getHash(i, offset);
            if (sourceMap[hash1]) {
                const hash2 = getHash(sourceMap[hash1].line, sourceMap[hash1].col);
                return `<a class="${getPrefix(isMin)}${hash2} ident" onclick="show(this, '${getPrefix(!isMin)}${hash1}')">${match}</a>`;
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
    const links = document.querySelectorAll("." + hash);
    removeHighlights();
    for (const link of links) {
        addHighlight(link);
    }
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

function updateShader(shaderSource, shaderMinified, traceEvents) {
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
    showTraceEvents(traceEvents || []);
}

// List what the minifier did, and mark the corresponding lines in the source view.
function showTraceEvents(events) {
    const source = document.querySelector('.source');
    const sourceLines = source.querySelectorAll('.line');
    const details = document.querySelector('.trace-details');
    const header = document.querySelector('.trace-header');
    const list = document.querySelector('.trace');
    list.innerHTML = '';
    // The log is collapsed by default, and hidden when there is nothing to show.
    details.hidden = events.length === 0;
    header.textContent = `Minifier log (${events.length})`;

    for (const event of events) {
        const item = document.createElement('div');
        item.classList.add('trace-event');

        const kind = document.createElement('span');
        kind.classList.add('trace-kind', 'trace-' + event.kind.toLowerCase());
        kind.textContent = event.kind;
        const loc = document.createElement('span');
        loc.classList.add('trace-loc');
        loc.textContent = event.line >= 0 ? `${event.line}:${event.col}` : '';
        const message = document.createElement('span');
        message.textContent = event.message;
        item.append(kind, loc, message);

        const lineDiv = event.line > 0 ? sourceLines[event.line - 1] : undefined;
        if (lineDiv) {
            lineDiv.classList.add('traced');
            lineDiv.title = (lineDiv.title ? lineDiv.title + '\n' : '') + `${event.kind}: ${event.message}`;
            item.classList.add('clickable');
            item.onclick = () => {
                removeHighlights();
                lineDiv.classList.add('highlight');
                highlights.push(lineDiv);
                // Scroll the source view only, so that the log stays visible.
                source.scrollTop = lineDiv.offsetTop - source.offsetTop - source.clientHeight / 2;
            };
        }
        list.appendChild(item);
    }
}

function copyBtn() {
    const minified = document.querySelector('.minified');
    // Each line is a separate div, so textContent would drop the newlines.
    const text = Array.from(minified.querySelectorAll('.line'), line => line.textContent).join('\n');
    navigator.clipboard.writeText(text);
    const icon = document.querySelector('.copy-icon');

    // animation
    icon.classList.remove('animated-icon');
    setTimeout(() => icon.classList.add('animated-icon'), 0);
}
