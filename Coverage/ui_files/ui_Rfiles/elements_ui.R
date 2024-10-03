jsCode <- paste0('shinyjs.clickBtn = function(params){
                    console.log("btn clicked");
                    var defaultParams = {
                       btnId : ""
                    };
                    params = shinyjs.getParams(params, defaultParams);
                    btn = $("#"+params.btnId);
                    console.log(btn);
                    btn[0].click();
                  }')

loaderJs <- '
shinyjs.setLoaderAppearance = function(tab){
  $("#shiny-notification-panel").addClass("custom-loader");
  $("#shiny-notification-panel").detach().prependTo("#loaderWrapper"+tab);
  }
'