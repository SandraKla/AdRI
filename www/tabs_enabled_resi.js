var tab_resi = $('a[data-value="nav_resi"]').parent().addClass("disabled");

             
                    $(function(){
                      $(tab_resi.parent()).on("click", "li.disabled", function(e) {
                        e.preventDefault();
                        return false;
                      });
                    });
                    