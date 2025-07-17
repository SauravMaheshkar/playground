/* eslint-disable require-jsdoc */
/* eslint-disable no-unused-vars */
let qtree;

function setup() {
  createCanvas(400, 400);
  background(255);
  const boundary = new Rectangle(200, 200, 200, 200);
  qtree = new QuadTree(boundary, 4);
  for (let i = 0; i < 300; i++) {
    const x = randomGaussian(width / 2, width / 8);
    const y = randomGaussian(height / 2, height / 8);
    const p = new Point(x, y);
    qtree.insert(p);
  }
}

function draw() {
  background(0);
  qtree.show();

  stroke(0, 255, 0);
  rectMode(CENTER);
  const range = new Rectangle(mouseX, mouseY, 25, 25);

  // This check has been introduced due to a bug discussed in https://github.com/CodingTrain/website/pull/556
  if (mouseX < width && mouseY < height) {
    rect(range.x, range.y, range.w * 2, range.h * 2);
    const points = qtree.query(range);
    for (const p of points) {
      strokeWeight(4);
      point(p.x, p.y);
    }
  }
}
