shinyjs.addCSS = function(cssFile){
  var link = document.createElement("link");
  link.href = cssFile;
  link.rel = "stylesheet";
  link.id = "themeCSS";
  document.head.appendChild(link);
}

shinyjs.removeCSS = function(){
  var old = document.getElementById("themeCSS");
  if (old) old.remove();
}