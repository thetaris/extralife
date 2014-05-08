$(function() {
  $('#main_plot').bind('DOMNodeInserted', function() {
    setTimeout(function() {
      $('#main_plot tr ').each(function(index, elt) {
        $(elt).click(function() {
        //$(elt).bind("click", function(){
           var tabs = $('.header+.tabbable>.nav.nav-tabs>li ')
      	 	 tabs.each(function() {
      			$(this).removeClass('active')
      	 	 })
      		 $(tabs[index]).addClass('active')
      		
      		 var tabsContents = $('.header+ .tabbable>.tab-content>.tab-pane')
           //$(' .tabbable .tab-content .tab-pane')
      	 	 tabsContents.each(function() {
      			$(this).removeClass('active')
      	 	 })
      		 $(tabsContents[index]).addClass('active')
      		
      		$($('.header+.tabbable>.nav.nav-tabs>li')[index]).trigger('change').trigger('shown');
      
        });
      });
    },10);
  });
});
