                                     Shiny.addCustomMessageHandler('createTable',function(data){
                                       var table = '<table border=\"1\" style=\"width:100%\">';
                                       table += '<tr><th>Name</th><th>Age</th><th>Job</th><th>Details</th></tr>';
                                       
                                       data.forEach(function(row,index){
                                        table += '<tr>class=\"data-row\" data-index=\"' + index + '\">';
                                        Object.values(row).forEach(function(value)) {
                                        table += '<td>' + value + '</td>';
                                        });
                                        table += '<td>' + Click to expand + '</td></tr>';
                                        
                                        table += '<tr class=\"expandable-row\" data-index=\"' + index + '\" style=\"display: none;'\">';
                                        
                                        table += '<td + colspan=\"4\">';
                                        table += '<table border=\"1\" style=\"width:100%\" class=\"nested-table\"><tr><th>Details 1</th></tr><tr><th>Details 1</th></tr>';
                                        
                                        table += '<tr<td> More about ' + row.name + '</td> <td> Additional Info</td></tr></table>';
                                        table += '</td></tr>';
                                        
                                       });
                                       
                                       table += '</table>';
                                       
                                       document.getElementByID('jsTable').innerHTML = table;
                                       //CLick Listeners
                                       document.querySelectorAll('.data-row').forEach(function(row) {

                                        row.addEventListener('click',function(){
                                            var index = this.getAttribute('data-index');
                                            var expandableRow = document.querySelector('.expandable-row[data-index=\"' + index + '\"]');
                                            if (expandableRow.style.display ==='none'){
                                                expandableRow.style.display = 'table-row'
                                            } else{
                                                expandableRow.style.display = 'none';
                                            }
                                        });
                                       });
                                       //
                                       document.querySelectorAll('.nested-table tr').forEach(function(row) {

                                        row.addEventListener('click',function(event){
                                            event.stopPropagation();
                                            this.classList.toggle('selected');
                                            if (this.classList.toggle('selected')) {
                                                this.style.backgroundColor = '#D6EAF8';
                                            } else{
                                                this.style.backgroundColor = '';
                                            }
                                            
                                        });
                                       });
                                       
                                       function toggleAllDetailRows(){
                                        var expandableRows = document.querySelectorAll('.expandable-row');
                                        var anyHidden = Array.from(expandableRows).some(row => row.style.display === 'none');
                                        expandableRows.forEach(function(row)) {
                                            row.style.display = anyHidden ? 'table-row' ; 'none';
                                        });
                                    }
                                       
                                          });"