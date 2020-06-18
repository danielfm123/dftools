valores = reactiveValues()

observe({
  if(input$password == "cci"){
    valores$login = T
    print("loged in!")
  }else{
    valores$login = F
  }
})

get_login_modal = function(){
  showModal(modalDialog(
    title = "Error de Autentificación",
    "Ingrese la Contraseña!"
  ))
}
