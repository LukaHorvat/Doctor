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
