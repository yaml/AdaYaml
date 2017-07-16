function highlightevent() {
  let elms = document.getElementsByClassName ('event');
  for (var i = 0; i < elms.length; i++) {
    elms.item (i).classList.remove ('mouseover');
  }
  var eventClass = null;
  for (var i = 0; i < this.classList.length; i++) {
    let curClass = this.classList.item (i);
    if (curClass.startsWith ("event") && curClass != "event") {
      eventClass = curClass;
      break;
    }
  }
  if (eventClass != null) {
    let highlightElms = document.getElementsByClassName (eventClass);
    for (var i = 0; i < highlightElms.length; i++) {
      highlightElms.item (i).classList.add ('mouseover');
    }
  }
}

function installHighlighter() {
  let elms = document.getElementsByClassName ('event');
  for (var i = 0; i < elms.length; i++) {
    elms.item (i).addEventListener ("mouseenter", highlightevent);
  }
}

window.onload = function() {
  installHighlighter();
  document.getElementById ("edit").addEventListener ('click', function(event) {
    var inspect = document.getElementById("yaml-inspect");
    if (inspect.classList.contains("edit")) {
      var xhr = new XMLHttpRequest();
      var input = document.getElementById("yaml-input");
      xhr.open ('GET', '?input=' + encodeURIComponent (input.childNodes[0].value));
      xhr.send (null);
      xhr.onreadystatechange = function () {
        var DONE = 4;
        var OK = 200;
        if (xhr.readyState === DONE) {
          if (xhr.status === OK)  {
            while (input.childNodes.length > 0) {
              input.removeChild (input.childNodes[0]);
            }
            // because JS is stupid
            var container = document.implementation.createHTMLDocument().documentElement;
            container.innerHTML = xhr.responseText;
            input.appendChild (container.querySelector ("#editable-input"));
            input.appendChild (container.querySelector ("#rendered-input"));
            var output = document.getElementById("yaml-output");
            while (output.childNodes.length > 0) {
              output.removeChild (output.childNodes[0]);
            }
            output.appendChild (container.querySelector ("#yaml-output").childNodes[0]);
            installHighlighter();
          } else {
            console.log ('Error: ' + xhr.status);
          }
          inspect.classList.remove ("edit");
          document.getElementById ("edit").innerText = "Edit";
        }
      };
    } else {
      inspect.classList.add ("edit");
      document.getElementById ("edit").innerText = "Submit";
    }
  });
  document.getElementById ("cancel").addEventListener ('click', function (event) {
    let inspect = document.getElementById ("yaml-inspect");
    inspect.classList.remove ("edit");
    document.getElementById ("edit").innerText = "Edit";
  });
}
