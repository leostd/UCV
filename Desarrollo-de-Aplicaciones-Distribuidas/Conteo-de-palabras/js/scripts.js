document.getElementById("CargarArchivo").onchange = function() {
  var text = document.getElementById("Archivo");
  var filename = this.value;
  var lastIndex = filename.lastIndexOf("\\");
    if (lastIndex >= 0) {
        filename = filename.substring(lastIndex + 1);
    }
  text.innerHTML = "Archivo Seleccionado: "+filename;
};
