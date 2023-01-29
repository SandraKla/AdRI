var tab = $('a[data-value="nav_lms"]').parent().addClass("disabled")
                    $(function(){
                      $(tab.parent()).on("click", "li.disabled", function(e) {
                        e.preventDefault();
                        return false;
                      });
                    });
					
					
var tabs = $('a[data-value="nav_gamlss"]').parent().addClass("disabled");

             
                    $(function(){
                      $(tabs.parent()).on("click", "li.disabled", function(e) {
                        e.preventDefault();
                        return false;
                      });
                    });