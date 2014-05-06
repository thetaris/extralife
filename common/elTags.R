elTags <- list(
  img = function(src, ...) {
    src = paste('/shiny/extralife/images', src, sep='/')
    tags$img(src = src, ...)
  },
  datasafe = function(text, task) {
    list(
      tags$button(class="graypage-open-datasave", "data-entry-task"=task, text),
      tags$script(type="text/javascript", src="/sites/all/modules/graypagegridster/actions.js")
    )
  },
  datasafe.float = function(text="Daten ändern", task=0) {
    tags$div(style = 'position:absolute; right:10px; top:10px', elTags$datasafe(text, task))
  }
)
