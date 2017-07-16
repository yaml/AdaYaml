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

window.onload = function() {
  let elms = document.getElementsByClassName ('event');
  for (var i = 0; i < elms.length; i++) {
    elms.item (i).addEventListener ("mouseenter", highlightevent);
  }
  document.getElementById ("edit").addEventListener ('click', function(event) {
    var inspect = document.getElementById("yaml-inspect");
    if (inspect.classList.contains("edit")) {
      var xhr = new XMLHttpRequest();
      var input = document.getElementById("yaml-input");
      xhr.open ('GET', '?input=' + encodeURI (input.childNodes[0].innerText));
      xhr.send (null);
      xhr.onreadystatechange = function () {
        var DONE = 4;
        var OK = 200;
        if (xhr.readyState === DONE) {
          if (xhr.status === OK)  {
            var children = input.childNodes;
            for (var i = 0; i < children.length; i++) {
              input.removeChild (children[i]);
            }
            input.appendChild (xhr.response.getElementById ("editable-input"));
            input.appendChild (xhr.response.getElementById ("rendered-input"));
            var output = document.getElementById("yaml-output");
            children = output.childNodes;
            for (var i = 0; i < children.length; i++) {
              output.removeChild (children[i]);
            }
            output.appendChild (xhr.response.getElementById ("yaml-output").childNodes[0]);
          } else {
            console.log ('Error: ' + xhr.status);
          }
          inspect.classList.remove ("edit");
        }
      };
    } else {
      inspect.classList.add ("edit");
    }
  });
}