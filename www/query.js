$(document).ready(function() {
  const elements = document.querySelectorAll('.element')
  
  elements.forEach(element => {
    element.addEventListener('click', () => Shiny.onInputChange("selected", element.id))
  })
  
})