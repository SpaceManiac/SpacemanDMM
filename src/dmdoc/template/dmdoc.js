// ----------------------------------------------------------------------------
// Index page tree browsing

document.addEventListener("DOMContentLoaded", function() {
    var items = document.getElementsByClassName("index-tree");
    var nodes = [];

    for (var i = 0; i < items.length; ++i) {
        var node = items[i];
        var parent = node.parentElement;
        if (!parent || parent.tagName.toLowerCase() != "li") {
            continue;
        }
        node.hidden = true;
        parent.style.listStyle = "none";
        var expander = document.createElement("span");
        expander.className = "expander";
        expander.textContent = "\u25B6";
        expander.addEventListener("click", function(node) {
            return function(event) {
                if (event.target.tagName.toLowerCase() == "a") {
                    return;
                }
                event.preventDefault();
                event.stopPropagation(true);
                node.hidden = !node.hidden;
                this.textContent = node.hidden ? "\u25B6" : "\u25BC";
            };
        }(node));
        parent.insertBefore(expander, parent.firstChild);
        nodes.push([node, expander]);
    }

    if (nodes.length) {
        var toggle = document.createElement("a");
        toggle.href = "#";
        toggle.appendChild(document.createTextNode("Toggle All"));
        toggle.addEventListener("click", function(event) {
            event.preventDefault();

            var hidden = !nodes[0][0].hidden;
            for (var i = 0; i < nodes.length; ++i) {
                nodes[i][0].hidden = hidden;
                nodes[i][1].textContent = hidden ? "\u25B6" : "\u25BC";
            }
        });

        var header = document.getElementsByTagName("header")[0];
        header.appendChild(document.createTextNode(" - "));
        header.appendChild(toggle);
    }
});
