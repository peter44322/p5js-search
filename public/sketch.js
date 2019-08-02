var grid = [["0", "0", "1", "0"], ["1", "0", "2", "0"], ["0", "0", "1", "0"]];
var session = pl.create();
let RectW = 300;
let RectH = 200;
let houseImg;
let forestImg;
let bobImg;

var solution;
var myCost = 5;
var leftCost = 1;
var rightCost = 2;
var upCost = 3;
var downCost = 4;

var holding = null;
function preload() {
  houseImg = loadImage("house.png");
  forestImg = loadImage("forest.png");
  bobImg = loadImage("bob.png");
}
function setup() {
  session.consult("app.pl");

  createCanvas(windowWidth, 400);

  let gui = createGui("Settings").setPosition(width - 500, 50);
  sliderRange(0, 20, 1);

  gui.addGlobals("myCost", "leftCost", "rightCost", "upCost", "downCost");
  //gui.moveTo(500, 100);

  // background(0);

  forstButton = new Clickable();
  forstButton.locate(600, 100);
  forstButton.text = "Forest";
  forstButton.onPress = function() {
    clearButtonsColors();
    this.color = "#AAAAFF";
    holding = "1";
  };

  houseButton = new Clickable();
  houseButton.locate(600, 160);
  houseButton.text = "House";
  houseButton.onPress = function() {
    clearButtonsColors();
    this.color = "#AAAAFF";
    holding = "0";
  };

  bobButton = new Clickable();
  bobButton.locate(600, 220);
  bobButton.text = "Bob";
  bobButton.onPress = function() {
    clearButtonsColors();
    this.color = "#AAAAFF";
    holding = "2";
  };

  solveButton = new Clickable();
  solveButton.locate(600, 280);
  solveButton.text = "Solve";
  solveButton.onPress = function() {
    clearButtonsColors();
    holding = null;
    let render = toString();
    console.log(render);

    session.query(
      `go(
    s([${render}], ${myCost}, [${leftCost}, ${rightCost}, ${upCost}, ${downCost}], 1),
    R).`
    );

    session.answer(x => {
      console.log(pl.format_answer(x));
      solution = x.links.R.toJavaScript();
      //alert(solution);
    });
  };
}

function draw() {
  background(255);
  fill(225);

  rect(100, 100, RectW, RectH);
  drawLines();

  forstButton.draw();
  houseButton.draw();
  bobButton.draw();
  solveButton.draw();

  for (var i = 0; i < 3; i++) {
    for (var j = 0; j < 4; j++) {
      var imageVar;
      if (grid[i][j] === "0") {
        imageVar = houseImg;
      } else if (grid[i][j] === "1") {
        imageVar = forestImg;
      } else if (grid[i][j] === "2") {
        imageVar = bobImg;
      }
      image(imageVar, 120 + j * (RectW / 4), 120 + i * (RectH / 3), 30, 30);
    }
  }
  textSize(20);

  text("Solution", 500, 100);
  if (solution) {
    text(solution, 500, 140);
  }
}

function drawLines() {
  stroke(51);
  line(100 + RectW / 4, 100, 100 + RectW / 4, 100 + RectH);
  line(100 + (RectW / 4) * 2, 100, 100 + (RectW / 4) * 2, 100 + RectH);
  line(100 + (RectW / 4) * 3, 100, 100 + (RectW / 4) * 3, 100 + RectH);

  line(100, 100 + RectH / 3, 100 + RectW, 100 + RectH / 3);
  line(100, 100 + (RectH / 3) * 2, 100 + RectW, 100 + (RectH / 3) * 2);
}

function clearButtonsColors() {
  forstButton.color = 255;
  houseButton.color = 255;
  bobButton.color = 255;
}

function mouseClicked() {
  for (var i = 0; i < 3; i++) {
    for (var j = 0; j < 4; j++) {
      if (
        collidePointRect(
          mouseX,
          mouseY,
          100 + j * (RectW / 4),
          100 + i * (RectH / 3),
          RectW / 4,
          RectH / 3
        ) &&
        holding
      ) {
        if (holding === "2") {
          removeBob();
        }
        if (grid[i][j] != "2") {
          grid[i][j] = holding;
        }
      }
    }
  }
}

function removeBob() {
  for (var i = 0; i < 3; i++) {
    for (var j = 0; j < 4; j++) {
      grid[i][j] === "2" ? (grid[i][j] = "0") : "";
    }
  }
}

function toString() {
  var render = "";
  for (var i = 0; i < 3; i++) {
    for (var j = 0; j < 4; j++) {
      lastChar = ",";
      if (j == 3 && i == 2) {
        lastChar = "";
      }
      render += grid[i][j] + lastChar;
    }
  }
  return render;
}
