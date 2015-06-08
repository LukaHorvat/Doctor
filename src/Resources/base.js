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
        rect.bottom > proseRect.top /*or $(window).height() */
    );
}

var repositionSnippets = function () {
    document.getElementsByClassName("prose-text")[0].style.minHeight =
        ((window.innerHeight || document.documentElement.clientHeight) + proseHeight) + "px";
    var refs = [].slice.call(document.getElementsByClassName("code-ref"));
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

var proseHeight = document.getElementsByClassName("prose")[0];

var load = function () {
    var proseText = document.getElementsByClassName("prose-text")[0];
    proseText.innerHTML = marked(proseText.innerHTML);
    proseHeight = document.getElementsByClassName("prose-text")[0].offsetHeight;
    document.getElementsByClassName("prose")[0].addEventListener("scroll", repositionSnippets);
    repositionSnippets();
}

document.addEventListener("DOMContentLoaded", load);
