function toggle(id) {
  var item = document.getElementById(id);
  if (item) {
    item.className = (item.className == 'hidden') ? 'visible' : 'hidden';
  }
}