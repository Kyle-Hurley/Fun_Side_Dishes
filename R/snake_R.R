library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), 
  tags$head(
    tags$script(HTML(
      "
      let snake_body = [{x: 300, y: 300}]; 
      let snake_direction = 'STOP'; 
      
      function check_collision(x, y, array) {
        for (let i = 1; i < array.length; i++) {
          if (array[i].x === x && array[i].y === y) {
            return true;
          }
        }
        return false;
      }
      
      // Snake head and body positioning
      function update_snake_position() {
        let new_head_position = {x: snake_body[0].x, y: snake_body[0].y};
        
        // Check for self-collision
        if (check_collision(new_head_position.x, new_head_position.y, snake_body)) {
          $('#game-over-message').show(); // Show game over message
          clearInterval(updateInterbval); // Stop the game
          return;
        }
        
        // Dynamic snake head position
        switch(snake_direction) {
          case 'up': new_head_position.y = Math.max(0, new_head_position.y - 20); break;
          case 'down': new_head_position.y = Math.min(580, new_head_position.y + 20); break;
          case 'left': new_head_position.x = Math.max(0, new_head_position.x - 20); break;
          case 'right': new_head_position.x = Math.min(580, new_head_position.x + 20); break;
        }

        // Update snake body positions here if the snake is longer

        // Eat an apple
        if (new_head_position.x === $('#apple').position().left 
        && new_head_position.y === $('#apple').position().top) {
          snake_body.push({...snake_body[snake_body.length - 1]});
          place_apple(); 
        }
        
        // Move snake body
        for (let i = snake_body.length -1; i > 0; i --) {
          snake_body[i] = {...snake_body[i - 1]};
        }

        // Update the snake's head position
        snake_body[0] = new_head_position;
        
        // On-screen snake body positions
        update_snake_body();
      }
      
      // function update_snake_body() {
        // $('#snake').css({left: snake_body[0].x + 'px', top: snake_body[0].y + 'px'});
      // }
      
      function update_snake_body() {
        // Remove existing snake segments to avoid duplications
        $('.snake-segment').remove();
    
        // Loop through each segment of the snake body
        snake_body.forEach((segment, index) => {
            // Create a new div element for the snake segment
            let segmentElement = $('<div></div>');
            segmentElement.addClass('snake-segment'); 
            segmentElement.css({ 
                'left': segment.x + 'px', 
                'top': segment.y + 'px',
                'position': 'absolute',
                'width': '20px', 
                'height': '20px', 
                'background-color': index === 0 ? 'brown' : 'darkgreen'
            });
    
            // Append the new segment to the game area
            $('#game-area').append(segmentElement);
        });
      }
      
      function place_apple() {
        let apple_x, apple_y, isOnSnake;
        do {
          isOnSnake = false;
          apple_x = Math.floor(Math.random() * 30) * 20;
          apple_y = Math.floor(Math.random() * 30) * 20;
          
          // Check if the apple's new position is on the snake body
          snake_body.forEach(segment => {
            if (segment.x === apple_x && segment.y === apple_y) {
              isOnSnake = true;
            }
          });
        } while (isOnSnake); // Keep trying new positions until it's not on the snake

        $('#apple').css({left: apple_x + 'px', top: apple_y + 'px'});
      }

      $(function () {
        place_apple();
        setInterval(update_snake_position, 200);
        
        $(document).on('keydown', function(e) {
          switch(e.key) {
            case 'ArrowUp': snake_direction = 'up'; break;
            case 'ArrowDown': snake_direction = 'down'; break;
            case 'ArrowLeft': snake_direction = 'left'; break;
            case 'ArrowRight': snake_direction = 'right'; break;
          }
        });
      })
      "
    ))
  ), 
  
  div(id = "game-area", style = "width: 600px; height: 600px; border: 1px solid black; 
      position: relative;", 
      div(id = "apple", style = "width: 20px; height: 20px; background-color: red; 
          position: absolute;"), 
      div(id = "game-over-message", style = "margin: 0; display: none; position: absolute; 
          top: 50%; left: 50%; -ms-transform: translate(-50%, -50%); 
          transform: translate(-50%, -50%); background-color: white; padding: 20px; 
          border-radius: 10px; text-align: center; z-index: 100;", 
          h2("Game Over!"), 
          actionButton("restart", "Restart", class = "btn-primary"))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$restart, {
    shinyjs::runjs('
     snake_body = [{x: 300, y: 300}];
     snake_direction = "STOP";
     $("#game-over-message").hide();
     place_apple();
     updateInterval = setInterval(update_snake_position, 200);
    ')
  })
  
}

shinyApp(ui = ui, server = server)
