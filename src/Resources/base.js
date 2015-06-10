var bgColor = function (col) { return function (el) {
    var ids = el.dataset.refId.split(",");
    ids.forEach(function (id) {
        var els = document.getElementsByClassName("ref-id-" + id);
        for (var i = 0; i < els.length; ++i) {
            var item = els[i];
            item.style.backgroundColor = col;
        }
    });
}};

var hover = bgColor("rgba(255, 255, 0, 0.3)");

var unhover = bgColor("");

function isElementInViewport (el) {
    var rect = el.getBoundingClientRect();
    var proseRect = document.getElementsByClassName("prose")[0].getBoundingClientRect();
    return (
        rect.top > proseRect.top
    );
}

var repositionSnippets = function () {
    document.getElementsByClassName("prose-text")[0].style.minHeight =
        ((window.innerHeight || document.documentElement.clientHeight) + proseHeight) + "px";
    var refs = [].slice.call(document.getElementsByClassName("code-ref"));
    var proseRect = document.getElementsByClassName("prose")[0].getBoundingClientRect();
    var minTop = refs.map(function (el) { return el.getBoundingClientRect().top - proseRect.top; })
                     .filter(function (x) { return x > 0; })
                     .reduce(function (m, x) { return Math.min(m, x) }, 1000000)
    document.getElementsByClassName("snippet-filler")[0].style.height = minTop + "px";
    refs.forEach(function (x) {
        x.dataset.refId.split(",").forEach(function (id) {
            var snips = [].slice.call(document.getElementsByClassName("ref-id-" + id));
            snips.forEach(function (snip) {
                if (isElementInViewport(x)) snip.style.display = "";
                else snip.style.display = "none";
            });
        });
    });
}

var proseHeight = 0;

function getCoords(elem) { // crossbrowser version
    var box = elem.getBoundingClientRect();

    var body = document.body;
    var docEl = document.documentElement;

    var scrollTop = window.pageYOffset || docEl.scrollTop || body.scrollTop;
    var scrollLeft = window.pageXOffset || docEl.scrollLeft || body.scrollLeft;

    var clientTop = docEl.clientTop || body.clientTop || 0;
    var clientLeft = docEl.clientLeft || body.clientLeft || 0;

    var top  = box.top +  scrollTop - clientTop;
    var left = box.left + scrollLeft - clientLeft;

    return { top: Math.round(top), left: Math.round(left) };
}

var overlayElement = null;

var cleanOverlay = function () {
    if (overlayElement != null) {
        document.body.removeChild(overlayElement);
        overlayElement = null;
    }
}

var snipHover = function (snip) {
    snip.addEventListener("mouseover", function () {
        cleanOverlay();
        overlayElement = snip.cloneNode(true);
        var coords = getCoords(snip);
        overlayElement.style.position = "absolute";
        overlayElement.style.top = coords.top + "px";
        overlayElement.style.left = coords.left + "px";
        overlayElement.style.opacity = "0";
        var interval = window.setInterval(function () {
            if (overlayElement == null) {
                window.clearInterval(interval);
                return;
            }
            var x = +overlayElement.style.opacity;
            if (x >= 1) window.clearInterval(interval);
            else overlayElement.style.opacity = "" + (x + 0.05);
        }, 16);
        overlayElement.className += " snippet-overlay";
        document.body.appendChild(overlayElement);
        overlayElement.addEventListener("mouseout", cleanOverlay);
    });
}

var load = function () {
    var proseText = document.getElementsByClassName("prose-text")[0];
    proseText.innerHTML = marked(proseText.innerHTML);
    proseHeight = document.getElementsByClassName("prose-text")[0].offsetHeight;
    document.getElementsByClassName("prose")[0].addEventListener("scroll", repositionSnippets);
    [].slice.call(document.getElementsByClassName("code-snippet")).forEach(snipHover);

    repositionSnippets();
}

document.addEventListener("DOMContentLoaded", load);
