<title>Network of Product Types-Subtypes Bought Together</title>
<h1>Network of Product Types-Subtypes Bought Together</h1>
Number of Orders:
  <RangeSlider range pips all="label" min={0} max={80000} step={10000} bind:values = {slider_value} on:change={updateGraph} />
Number of times they are ordered together:
  <RangeSlider range pips all="label" min={0} max={12500} step={500} bind:values = {slider_value_link} on:change={updateGraph} />

  <script>
	import {onMount} from 'svelte';
	import * as d3 from 'd3';
	import RangeSlider from "svelte-range-slider-pips";
	let slider_value = [0,80000];
	let slider_value_link = [0,12500];

		
	let width = 1500;
	let element;

	onMount(async function() {
		let data = await d3.json('https://raw.githubusercontent.com/trangtph/learning_Visualization/main/data-viz-project/src/routes/product_link/network_typesub.json');
		console.log(data);
		
		let chart = ForceGraph(data, {
			nodeId: d => d.index,
			nodeGroup: d => d.group,
			nodeTitle: d => `${d.name}`,
			linkStrokeWidth: l => (l.value/3000)*1.5,
			nodeRadius: d => d.degree/1000,
			nodeOpacity: d => d.degree >= slider_value[0] && d.degree < slider_value[1] ? 1 : 0.5,
			width,
			height: 650,
		});
		
		d3.select(element).append(() => chart);
	});
	
	// Copyright 2021 Observable, Inc.
	// Released under the ISC license.
	// https://observablehq.com/@d3/force-directed-graph
	function ForceGraph({
		nodes, // an iterable of node objects (typically [{id}, …])
		links // an iterable of link objects (typically [{source, target}, …])
	}, {
		nodeId = d => d.id, // given d in nodes, returns a unique identifier (string)
		nodeGroup, // given d in nodes, returns an (ordinal) value for color
		nodeGroups, // an array of ordinal values representing the node groups
		nodeTitle, // given d in nodes, a title string
		nodeFill = "currentColor", // node stroke fill (if not using a group color encoding)
		nodeStroke = "#fff", // node stroke color
		nodeStrokeWidth = 0, // node stroke width, in pixels
		nodeStrokeOpacity = 1, // node stroke opacity
		nodeRadius, // node radius, in pixels
		nodeOpacity = 1,
		nodeStrength,
		linkSource = ({source}) => source, // given d in links, returns a node identifier string
		linkTarget = ({target}) => target, // given d in links, returns a node identifier string
		linkStroke = "#999", // link stroke color
		linkStrokeOpacity = 1, // link stroke opacity
		linkStrokeWidth = 1.5, // given d in links, returns a stroke width in pixels
		linkStrokeLinecap = "round", // link stroke linecap
		linkStrength,
		colors = d3.schemeObservable10, // an array of color strings, for the node groups
		width = 700, // outer width, in pixels
		height = 600, // outer height, in pixels
		invalidation // when this promise resolves, stop the simulation
	} = {}) {
		// Compute values.
		const N = d3.map(nodes, nodeId).map(intern);
		const R = typeof nodeRadius !== "function" ? null : d3.map(nodes, nodeRadius);
		const O = typeof nodeOpacity !== "function" ? null : d3.map(nodes, nodeOpacity);
		const LS = d3.map(links, linkSource).map(intern);
		const LT = d3.map(links, linkTarget).map(intern);
		if (nodeTitle === undefined) nodeTitle = (_, i) => N[i];
		const T = nodeTitle == null ? null : d3.map(nodes, nodeTitle);
		const G = nodeGroup == null ? null : d3.map(nodes, nodeGroup).map(intern);
		const W = typeof linkStrokeWidth !== "function" ? null : d3.map(links, linkStrokeWidth);
		const L = typeof linkStroke !== "function" ? null : d3.map(links, linkStroke);

		// Replace the input nodes and links with mutable objects for the simulation.
		nodes = d3.map(nodes, (_, i) => ({id: N[i]}));
		links = d3.map(links, (_, i) => ({source: LS[i], target: LT[i]}));

		// Compute default domains.
		if (G && nodeGroups === undefined) nodeGroups = d3.sort(G);

		// Construct the scales.
		const color = nodeGroup == null ? null : d3.scaleOrdinal(nodeGroups, colors);

		// Construct the forces.
		const forceNode = d3.forceManyBody();
		const forceLink = d3.forceLink(links).id(({index: i}) => N[i]);
		if (nodeStrength !== undefined) forceNode.strength(nodeStrength);
		if (linkStrength !== undefined) forceLink.strength(linkStrength);

		const simulation = d3.forceSimulation(nodes)
				.force("link", forceLink)
				//.force("charge", forceNode)
				.force("charge", d3.forceManyBody().strength(-1000))
				.force("center",  d3.forceCenter())
				.force("collision", d3.forceCollide())
				.on("tick", ticked);

		const svg = d3.create("svg")
				.attr("width", width)
				.attr("height", height)
				.attr("viewBox", [-width / 2, -height / 2, width, height])
				.attr("style", "max-width: 100%; height: auto; height: intrinsic;");

		const link = svg.append("g")
				.attr("stroke", typeof linkStroke !== "function" ? linkStroke : null)
				.attr("stroke-opacity", linkStrokeOpacity)
				.attr("stroke-width", typeof linkStrokeWidth !== "function" ? linkStrokeWidth : null)
				.attr("stroke-linecap", linkStrokeLinecap)
			.selectAll("line")
			.data(links)
			.join("line");

		const node = svg.append("g")
				.attr("fill", nodeFill)
				.attr("stroke", nodeStroke)
				.attr("stroke-opacity", nodeStrokeOpacity)
				.attr("stroke-width", nodeStrokeWidth)
    .selectAll("circle")
    .data(nodes)
     .join("circle")
    .attr("r", nodeRadius)
	.style("opacity", nodeOpacity)
    .call(drag(simulation));

	const textElems = svg
    .append('g')
    .selectAll('text')
    .data(nodes)
    .join('text')
    .text(({index: i}) => T[i])
    .attr('font-size', 12)
	.style('font-family', 'Arial')
    .call(drag(simulation));

		if (W) link.attr("stroke-width", ({index: i}) => W[i]);
		if (L) link.attr("stroke", ({index: i}) => L[i]);
		if (G) node.attr("fill", ({index: i}) => color(G[i]));
		if (R) node.attr("r", ({index: i}) => R[i]);
		if (O) node.style("opacity", ({index: i}) => O[i]);
		if (T) node.append("title").text(({index: i}) => T[i]);
		if (invalidation != null) invalidation.then(() => simulation.stop());

var legendWidth = 400; // Adjust the width of the legend as needed
var legendX = (width - legendWidth) / 2; // Calculate the x-coordinate for centering the legend

var keys = ['Adventuring Equipment ',
'Animals & Transportation ',
'Arms & Armour ',
'Jewelry ',
'Musical Instrument ',
'Potions & Scrolls ',
'Summoning Device ',
'Tools & Kits '];

const colorss = d3.schemeObservable10.slice(0, keys.length); // Use D3's category color scheme

const colorScale = d3.scaleOrdinal()
  .domain(keys)
  .range(colorss);

  var legend = svg.selectAll(".legend")
  .data(keys)
  .enter().append("g")
  .attr("class", "legend")
  .attr("transform", function(d, i) { return "translate(" + legendX + "," + (i * 20 + 20) + ")"; });

legend.append("rect")
  .attr("x", 0)
  .attr("width", 18)
  .attr("height", 18)
  .style("fill", d => colorScale(d));

legend.append("text")
  .attr("x", 24)
  .attr("y", 9)
  .attr("dy", ".35em")
  .style("text-anchor", "start")
  .text(d => d);

		function intern(value) {
			return value !== null && typeof value === "object" ? value.valueOf() : value;
		}

		function ticked() {
			link
				.attr("x1", d => d.source.x)
				.attr("y1", d => d.source.y)
				.attr("x2", d => d.target.x)
				.attr("y2", d => d.target.y);

			//node.attr("transform", (d) => `translate(${d.x} ${d.y})`);

			node
				.attr("cx", d => d.x)
				.attr("cy", d => d.y);

			textElems
      			.attr("x", d => d.x)
      			.attr("y", d => d.y);
		}

		function drag(simulation) {    
			function dragstarted(event) {
				if (!event.active) simulation.alphaTarget(0.3).restart();
				event.subject.fx = event.subject.x;
				event.subject.fy = event.subject.y;
			}

			function dragged(event) {
				event.subject.fx = event.x;
				event.subject.fy = event.y;
			}

			function dragended(event) {
				if (!event.active) simulation.alphaTarget(0);
				event.subject.fx = null;
				event.subject.fy = null;
			}

			return d3.drag()
				.on("start", dragstarted)
				.on("drag", dragged)
				.on("end", dragended);
		}

		return Object.assign(svg.node(), {scales: {color}});

};

	async function updateGraph() {
    const nodes = d3.selectAll("circle");
    nodes.each(function() {
		const nodeR = +d3.select(this).attr("r"); 
      // Adjust opacity based on comparison with slider value
      	const opacity = nodeR * 1000 >= slider_value[0] && nodeR * 1000 < slider_value[1] ? 1 : 0.3;
      	d3.select(this).style('opacity', opacity); // Update opacity of the node
		//d3.select(this).select("text").attr('font-size', 12 * opacity);
    });
	const links = d3.selectAll("line");
	links.each(function() {
    	const linkSt = +d3.select(this).attr("stroke-width");
    	const stroke = linkSt * 3000 >= slider_value_link[0] && linkSt * 3000 < slider_value_link[1] ? 1 : 0;
    	d3.select(this).attr("opacity", stroke);
	});
}

</script>

<div bind:this={element}>	
</div>

<style>

</style>