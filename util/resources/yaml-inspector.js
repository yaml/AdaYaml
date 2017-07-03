function highlightevent() {
  let elms = document.getElementsByClassName('event');
  for (var i = 0; i < elms.length; i++) {
    elms.item(i).classList.remove('mouseover');
  }
  var eventClass = null;
  for (var i = 0; i < this.classList.length; i++) {
    let curClass = this.classList.item(i);
    if (curClass.startsWith("event") && curClass != "event") {
      eventClass = curClass;
      break;
    }
  }
  if (eventClass != null) {
    let highlightElms = document.getElementsByClassName(eventClass);
    for (var i = 0; i < highlightElms.length; i++) {
      highlightElms.item(i).classList.add('mouseover');
    }
  }
}

window.onload = function() {
  let elms = document.getElementsByClassName('event');
  for (var i = 0; i < elms.length; i++) {
    elms.item(i).addEventListener("mouseenter", highlightevent);
  }
}