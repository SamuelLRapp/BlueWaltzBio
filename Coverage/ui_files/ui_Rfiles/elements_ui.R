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

tst <- paste0('shinyjs.ncbiDwnFastaTest = function(params){
                var defaultParams = {
                   dwnBtnId : "fileDownloadF",
                   waitTimeForFileDwn: 40000,
                   testOrganisms: "gallus gallus",
                   testBarcodes: "(CO1; COI; COX1)"
                };
                params = shinyjs.getParams(params, defaultParams);
                
                //set search params
                Shiny.setInputValue("NCBIorganismList", params.testOrganisms);
                Shiny.setInputValue("barcodeList", params.testBarcodes);
                shinyjs.clickBtn("NCBIsearchButton"); //run search
  
                //loop to check if search complete
                var dwnPanelVis = false;
                console.log(dwnPanelVis);
                var intr = setInterval(function() {
                    
                  dwnPanelVis = $("#"+params.dwnBtnId).is(":visible");
                  //run this code after search completed
                  if (dwnPanelVis) {
                    clearInterval(intr);
                    console.log("loop finished");
                    shinyjs.clickBtn(params.dwnBtnId);
                    setTimeout(function(){
                      //let server know to check if file has downloaded
                        Shiny.onInputChange("ui-test-complete", true);
                    }, params.waitTimeForFileDwn);
                  }
                }, 1000)
              }')
loaderJs <- '
shinyjs.setLoaderAppearance = function(tab){
  $("#shiny-notification-panel").addClass("custom-loader");
  $("#shiny-notification-panel").detach().prependTo("#loaderWrapper"+tab);
  }
'