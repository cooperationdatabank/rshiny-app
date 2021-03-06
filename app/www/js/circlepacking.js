var diameter = Math.min(width, height),
    g = svg.append("g").attr(
      "transform",
      "translate(" + (width + 700 - diameter) / 2 + "," + (height + 800 - diameter) / 2 + ")"),
    format = d3.format(",d");

var pack = d3.pack()
    .size([diameter - 4, diameter - 4]);

var margin = 20 
	
var color = d3.scaleLinear()
    .domain([-1, 5])
	.range(["hsl(329,54%,47%)","hsl(61,94%,87%)"])
    //.range(["hsl(152,80%,80%)", "hsl(228,30%,40%)"])
    .interpolate(d3.interpolateHcl);

var mytooltip = svg
	.append("div")
	.attr("id","hover-label")
	.style("position", "absolute")
	.style("z-index", "10")
	.style("visibility", "hidden")
	
r2d3.onRender(function(root, svg, width, height, options) {
	
	
  root = d3.hierarchy(root)
      .sum(function(d) { return d.size; })
      .sort(function(a, b) { return b.value - a.value; });

  var focus = root,
	  nodes = pack(root).descendants(),
	  view;
	  
	  
  var circle = g.selectAll("circle")
    .data(nodes)
    .enter().append("circle")
      .attr("class", function(d) { return d.parent ? d.children ? "node" : "node node--leaf" : "node node--root"; })
      .style("fill", function(d) { return d.children ? color(d.depth) : null; })
      .on("click", function(d) { if (focus !== d) zoom(d), d3.event.stopPropagation(); })
	   .on("mouseover", function(d){
		   console.log(d.data.desc);
		   return mytooltip.text(d.data.desc).style("visibility", "visible");
	   })
	  	.on("mousemove", function(d){
			return mytooltip.style("top", '600px').style("left", "600px");
		})
	  	.on("mouseout", function(){return tooltip.style("visibility", "hidden");});

	var text = g.selectAll("text")
	    .data(nodes)
	    .enter().append("text")
	      .attr("class", "label")
	      .style("fill-opacity", function(d) { return d.parent === root ? 1 : 0; })
	      .style("display", function(d) { return d.parent === root ? "inline" : "none"; })
	      .text(function(d) { return d.data.name; });

  var node = g.selectAll("circle,text");
	  
 zoomTo([root.x, root.y, root.r * 2 + margin]);
 
 
 svg 
 
     // .style("background", color("-1"))
      .on("click", function() { zoom(root); });
 
  function zoom(d) {
	 
	   
    var focus0 = focus; 
	 focus = d;

    var transition = d3.transition()
        .duration(d3.event.altKey ? 7500 : 750)
        .tween("zoom", function(d) {
          var i = d3.interpolateZoom(view, [focus.x, focus.y, focus.r * 2 + margin]);
          return function(t) { zoomTo(i(t)); };
        });
 
    g.selectAll("text")
      .filter(function(d) { 
		  // console.log(d.data.name);
		  return d.parent === focus || this.style.display === "inline"; 
	  })
	  .transition(transition)
        .style("fill-opacity", function(d) {
  		   return d.parent === focus ? 1 : 0; 
		})
		// .style("display", function(d) {
// 			return d.parent === focus ? "inline" : "none";
// 		});
        .on("start", function(d) {
			if (d.parent === focus) this.style.display = "inline";
		})
        .on("end", function(d) { if (d.parent !== focus) this.style.display = "none"; });
	  
	   
    
  }

  function zoomTo(v) {
    var k = diameter / v[2]; view = v;
    node.attr("transform", function(d) { return "translate(" + (d.x - v[0]) * k + "," + (d.y - v[1]) * k + ")"; });
    circle.attr("r", function(d) { return d.r * k; });
  }
  
  // function hovered(hover) {
//     return function(d) {
//   	d3.selectAll(d.ancestors().map(function(d) {}));
//     };
//   }
 
});



