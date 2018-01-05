#' shows simple pop up message
#'
#' This function shows a simple dhtmlx alert
#' @param session Shiny session object from the server.r page of the app that will use this function
#' @param message Message to display in the box
#' @param title Title of the messagebox
#' @param okText Text to put on the 'OK' button. Default is 'OK'
#' @keywords popup modal dhtmlx alert
#' @export
showPopup <- function(session,message,title="",okText="OK") {
  
  # Remove " from string (these are sometimes present in warnings/error messages)
  # Keeping them in could result in faulty javascript code
  message <- gsub('"','\'',message)
  
  if(title!="")
    popupCode <- paste0('dhtmlx.alert({
                        title:"',title,'",
                        ok:"',okText,'",
                        text:"',message,'"})')
  else
    popupCode <- paste0('dhtmlx.alert({
                        ok:"',okText,'",
                        text:"',message,'"})')
  
  session$sendCustomMessage(type="jsCode",
                            list(code=popupCode))  
  }

#' shows a simple splash screen
#'
#' shows splash screen. This screen will remain visible and on top until hideSplash is called
#' @param session Shiny session object from the server.r page of the app that will use this function
#' @param html Html code to put on the splash screen
#' @param formName Id of the splash screen. Used to determine if a screen is already created.
#' @keywords splash popup modal dhtmlx alert
#' @export 
showSplash <- function(session,html='<h1>Please wait...</h1>',formName='splash') {
  
  html <- paste0("<div id='splash' style='display:none'> ",html,"</div>")
  
  modalCode <- paste0(' 
                      
                      if(typeof ',formName,' == "undefined"){   
                      var fragment = create("',html,'");
                      document.body.insertBefore(fragment, document.body.childNodes[0]);
                      var ',formName,' = dhtmlx.modalbox({ content:"splash", hidden:true });
                      
                      dhtmlx.modalbox(',formName,')
                      };
                      ')
  
  session$sendCustomMessage(type="jsCode",list(code=modalCode))
  
}

### Hides splash screen created with showSplash
#'
#' Hides a spalsh screen created with showSplash. 
#' @param session Shiny session object from the server.r page of the app that will use this function
#' @param formName Id of the form to hide
#' @keywords hide splash popup modal dhtmlx alert
#' @export 
hideSplash <- function(session,formName='splash') {
  
  html <- "<div id='splash' style='display:none'> <H1>Load data....<H1></div>"
  
  modalCode <- paste0('dhtmlx.modalbox.hide(',formName,');')
  
  session$sendCustomMessage(type="jsCode",list(code=modalCode)) 
}


#' Shows a confirm message.
#'
#' Shows a confirm message in the browser and returns result in input$inputName
#' @param session Shiny session object from the server.r page of the app that will use this function
#' @param inputName Name of the input to return the result to.
#' @param message Message to display on the confirm box
#' @keywords show confirm popup modal dhtmlx alert
#' @export 
showConfirm <- function(session,inputName,message,okText="OK",cancelText="Cancel") {
  
  ### We create a global counter var for this message box. If 'inputName' is name spaced from a module it will contain a '-'
  ### We need to replace the '-' with a '_' to make the name a valid variable name in javascript
  jInputName <- gsub("-","_",inputName,fixed=TRUE)
  
  confirmCode = paste0('

                      if (typeof(',jInputName,'_counter) == "undefined"){
                        ',jInputName,'_','counter = 0;
                      }

                       dhtmlx.confirm({
                       type:"confirm",
                       text: "',message,'",
                       ok: "',okText,'",
                       cancel: "',cancelText,'",
                       callback: function(result){
                       
                         if(result){
                         ',jInputName,'_','counter++;
                          Shiny.onInputChange("',inputName,'", ',jInputName,'_','counter);
                         }
                       }
                      });')
  
  session$sendCustomMessage(type="jsCode",list(code=confirmCode))
}

#' Shows a temporary message.
#' 
#' Shows a temporary message on the right of the screen.
#' @param session Shiny session object from the server.r page of the app that will use this function.
#' @param message Message to display.
#' @param type On of the perdefined message box types (see http://docs.dhtmlx.com/message__working_with_windows.html) or a custom css class
#' @keywords show confirm popup modal dhtmlx alert
#' @export 
showMessage <- function(session,message, type="message") {
  
  confirmCode = paste0('dhtmlx.message({
                       type:"',type,'",
                       text: "',message,'"
});')
    
  session$sendCustomMessage(type="jsCode",list(code=confirmCode))
  }

#' Shows a modal with custom html in it
#' 
#' Shows a modal with custom html in it and sends result back to input$inputName when an ok / cancel button is pressed
#' @param session Shiny session object from the server.r page of the app that will use this function.
#' @param inputName  Name of the input to return the result to.
#' @param html Html to put on the modal
#' @param okText Text to put on the ok button
#' @param cancelText Text to put on the cancel button
#' @param modalStyle Style sheet properties of the modal
#' @param buttonStyle Style sheet properties of the button
#' @keywords show confirm popup modal dhtmlx alert
#' @export 
showFormModal <- function(session,inputName,html,okText='ok',cancelText='cancel',modalStyle='display:none',buttonStyle='width:50x;padding:'){
  
  html <- gsub('"','\'',html)
  
  # formID is the value of the id tag for the div that will contain the modal
  formID   <- paste(inputName,'_id')
  # formName is the javascript variable name that will contain a reference to the modalbox object containing the form
  formName <- paste0(inputName,'_form')
  # Callbackname is the name of the call back function for the form
  CallbackName <- paste0(inputName,'_form_save_callback')
  
  # Wrap the given html in a div with an ok and cancel button
  strHtml <- paste0("<div id='",formID,"' style='",modalStyle,"'>",html,"
                    <div>
                    <span class='dhtmlx_button'><input type='button' value='",okText,"' onclick='",CallbackName,"(this)' style='",buttonStyle,"'></span>
                    <span class='dhtmlx_button'><input type='button' value='",cancelText,"' onclick='dhtmlx.modalbox.hide(this)' style='",buttonStyle,"'></span>
                    </div>
                    </div>")
  
  # Replace \n with \\n. An extra \ needs to be added for valid multiline javascript strings
  html <- gsub("\n","\\\\n",strHtml)
  
  # Build java script code to be sent to the client
  modalCode <- paste0(' 
                      
                      // Check if modal object exists
                      
                      if(typeof ',formName,' == "undefined"){   
                      
                      // Create a docuemnt fragment object with the forms html and insert it in the body of the main page if it is not created yet
                      
                      var fragment = create("',html,'");
                      document.body.insertBefore(fragment, document.body.childNodes[0]);
                      var ',formName,' = dhtmlx.modalbox({ content:"',formID,'", hidden:true });
                      
                      // Define callback function for the form.
                      // This function itterates over all input elements of the form and creates a Json with the content
                      // The Json is then sent to the server to input$inputName
                      
                      window.',CallbackName,' = function(box){
                      dhtmlx.modalbox.hide(box);
                      
                      var inputs  = ',formName,'.querySelectorAll("input, select");        
                      
                      var nInputs = inputs.length;
                      var values  = {};      
                      var value   = "";
                      
                      for (i = 0; i < nInputs; i++) { 
                      value  = inputs[i].value
                      name   = inputs[i].id          
                      values[name] = value
                      }
                      
                      Shiny.onInputChange("',inputName,'", values)
                      }
                      
                      // Show the modal
                      dhtmlx.modalbox(',formName,')
                      };
                      ')
  
  session$sendCustomMessage(type="jsCode",list(code=modalCode))
}

#' Adds includes for messagebox to page Should be called once on top of ui.r
#' 
#' @param style Stylesheet to use for the messagebox. Messagebox.js default comes with:
#'  message_default.css
#'  message_growl_dark.css
#'  message_growl_shiny.css
#'  message_skyblue.css
#'  message_solid.css
#' @keywords messagebox.js includes
#' @export 
addMessageBoxToPage <- function(style="message_default.css"){
  
 
  www    <- system.file('www', package='FrissMessageBox')
  
  msgFile <- paste0(www,"/message.js")
  cssFile <- paste0(www,"/themes/",style)
  
  lstRet <- list(addMessageHandler(),singleton(includeScript(msgFile)),singleton(includeCSS(cssFile)))
  return(lstRet)
}

#' Adds javascript message handler to the page
#' @keywords javascript message handler
#' @export
addMessageHandler <- function() {
  
  return(tags$head(tags$script(HTML('
                                    Shiny.addCustomMessageHandler("jsCode",
                                    function(message) {
                                    console.log(message)
                                    eval(message.code);
                                    }
                                    );
                                    function create(htmlStr) {
                                    var frag = document.createDocumentFragment(),
                                    temp = document.createElement("div");
                                    temp.innerHTML = htmlStr;
                                    while (temp.firstChild) {
                                    frag.appendChild(temp.firstChild);
                                    }
                                    return frag;
                                    }
                                    '))))
}