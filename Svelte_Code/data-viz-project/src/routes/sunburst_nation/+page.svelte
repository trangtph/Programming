<h1> Number of customers and total revenue per nation</h1>

<script>
	import { onMount } from "svelte";
	import L from "leaflet";
	import * as d3 from 'd3';
	import { scaleLinear } from 'd3-scale';


	const scaleR = scaleLinear().domain([0, 160]).range([0, 50]);
	const scaleColour = scaleLinear().domain([1000000, 160000000]).range(["#fff5f0", "#67000d"]);
	const scaleR2 = scaleLinear().domain([1000000, 160000000]).range([0, 70]);
	const scaleColour2 = scaleLinear().domain([0, 160]).range(["#fff5f0", "#67000d"]);
	const color_region = d3.scaleOrdinal(["Amnian Empire", "Bloodlands Empire", "Calim Empire",
	  "Dwarven Empire", "Eastern Empire", "Halruaan Empire",
	  "New Neverwinter Empire", "None Empire", "Old Empire",
	  "Purple Dragon Empire", "Southern Empire", "Thayan Empire",
	  "United Moonshae Empire", "Waterdhavian Empire", "Western Empire"],
	  ["#2673a3", "#b0799a", "#ef8737",
		"#355828", "#6c5d9e", "#bf3729",
		"#e48171", "#f5bb50", "#9d9cd5",
		"#17154f", "#f6b3b0", "#ada43b",
		"#8b3a2b", "#b38711", "#9cc184"]);
  
		onMount(async function () {
    let data = await d3.csv('https://raw.githubusercontent.com/trangtph/Programming/main/Processed_data/nation_customer_revenue.csv');
    let data2 = await d3.json('https://raw.githubusercontent.com/trangtph/Programming/main/Processed_data/top_product_nation_final.json');
    var map = L.map('map', { crs: L.CRS.Simple, minZoom: -1 });

    var imageUrl = 'https://github.com/trangtph/Map_Forgotten_Realms/blob/main/Map_Forgotten_Realms_off.png?raw=true';
    var imageBounds = [[0, 0], [660, 1000]];
    var image = L.imageOverlay(imageUrl, imageBounds).addTo(map);
	map.setMaxBounds(imageBounds);
	map.fitBounds(imageBounds);

	var chart;

    for (var i in data) {
        var row = data[i];
        var circle_color = scaleColour2(row.n_customer);
        var radius_transform = scaleR2(row.revenue);
        var border_transform = color_region(row.Region);
        var pop_up_text = 'Nation: ' + row.Nation + '<br>' + 'Region: ' + row.Region +
            '<br>' + 'Number of customer: ' + row.n_customer + ', rank ' + row.rank_n_customer +
            '<br>' + 'Total revenue: ' + row.revenue_round + ' Million CP' + ', rank ' + row.rank_revenue;

        var circle = L.circle([row.y_coord, row.x_coord], {
            radius: radius_transform,
            color: border_transform,
            fillColor: circle_color,
            fillOpacity: 0.8
        });
        circle.bindTooltip(pop_up_text);
		circle.nation = row.Nation;

		circle.on('click', function () {

            //var clickedNation = row.Nation;
            var filteredData = filterByName(data2, this.nation);

        //var chart = Sunburst(filteredData, {
		var chart = Sunburst(filteredData, {
                value: d => d.Percentage,
                label: d => d.name,
                title: (d, n) => `${n.ancestors().reverse().map(d => d.data.name).join(".")}\n${n.value.toLocaleString("en")}`,
                width: 1000,
                height: 1000
            });
			d3.select('#chart').selectAll("*").remove();
            d3.select('#chart').append(()=>chart);
		});
        circle.addTo(map);

	} 
})
function filterByName(node, nameToFilter) {
			if (node.name === nameToFilter) {
                return node;
            }
            if (node.children) {
                for (let i = 0; i < node.children.length; i++) {
                    let result = filterByName(node.children[i], nameToFilter);
                    if (result) {
                        return result;
                    }
                }
            }
            return null;
        }
		function Sunburst(data, { // data is either tabular (array of objects) or hierarchy (nested objects)
		path, // as an alternative to id and parentId, returns an array identifier, imputing internal nodes
		id = Array.isArray(data) ? d => d.id : null, // if tabular data, given a d in data, returns a unique identifier (string)
		parentId = Array.isArray(data) ? d => d.parentId : null, // if tabular data, given a node d, returns its parent’s identifier
		children, // if hierarchical data, given a d in data, returns its children
		value, // given a node d, returns a quantitative value (for area encoding; null for count)
		sort = (a, b) => d3.descending(a.value, b.value), // how to sort nodes prior to layout
		label, // given a node d, returns the name to display on the rectangle
		title, // given a node d, returns its hover text
		link, // given a node d, its link (if any)
		linkTarget = "_blank", // the target attribute for links (if any)
		width = 100, // outer width, in pixels
		height = 100, // outer height, in pixels
		margin = 50, // shorthand for margins
		marginTop = margin, // top margin, in pixels
		marginRight = margin, // right margin, in pixels
		marginBottom = margin, // bottom margin, in pixels
		marginLeft = margin, // left margin, in pixels
		padding = 1, // separation between arcs
		radius = Math.min(width - marginLeft - marginRight, height - marginTop - marginBottom) / 2, // outer radius
		color = d3.interpolateWarm, // color scheme, if any
		fill = "#ccc", // fill for arcs (if no color encoding)
		fillOpacity = 0.6, // fill opacity for arcs
	} = {}) {

		// If id and parentId options are specified, or the path option, use d3.stratify
		// to convert tabular data to a hierarchy; otherwise we assume that the data is
		// specified as an object {children} with nested objects (a.k.a. the “flare.json”
		// format), and use d3.hierarchy.
		const root = path != null ? d3.stratify().path(path)(data)
				: id != null || parentId != null ? d3.stratify().id(id).parentId(parentId)(data)
				: d3.hierarchy(data, children);

		// Compute the values of internal nodes by aggregating from the leaves.
		value == null ? root.count() : root.sum(d => Math.max(0, value(d)));

		// Sort the leaves (typically by descending value for a pleasing layout).
		if (sort != null) root.sort(sort);

		// Compute the partition layout. Note polar coordinates: x is angle and y is radius.
		d3.partition().size([2 * Math.PI, radius])(root);

		// Construct a color scale.
		if (color != null) {
			color = d3.scaleSequential([0, root.children.length - 1], color).unknown(fill);
			root.children.forEach((child, i) => child.index = i);
		}

		// Construct an arc generator.
		const arc = d3.arc()
				.startAngle(d => d.x0)
				.endAngle(d => d.x1)
				.padAngle(d => Math.min((d.x1 - d.x0) / 2, 2 * padding / radius))
				.padRadius(radius / 2)
				.innerRadius(d => d.y0)
				.outerRadius(d => d.y1 - padding);

		const svg = d3.create("svg")
				.attr("viewBox", [
					marginRight - marginLeft - width / 2,
					marginBottom - marginTop - height / 2,
					width,
					height
				])
				.attr("width", width)
				.attr("height", height)
				.attr("style", "max-width: 100%; height: auto; height: intrinsic;")
				.attr("font-family", "sans-serif")
				.attr("font-size", 19)
				.attr("text-anchor", "middle");

		const cell = svg
			.selectAll("a")
			.data(root.descendants())
			.join("a")
				.attr("xlink:href", link == null ? null : d => link(d.data, d))
				.attr("target", link == null ? null : linkTarget);

		cell.append("path")
				.attr("d", arc)
				.attr("fill", color ? d => color(d.ancestors().reverse()[1]?.index) : fill)
				.attr("fill-opacity", fillOpacity);

				if (label != null) {
  cell.filter(d => (d.y0 + d.y1) / 2 * (d.x1 - d.x0) > 10)
    .append("text")
    .attr("dy", "0.32em")
    .attr("transform", d => {
      const [x, y] = arc.centroid(d);
      return `translate(${x}, ${y})`;
    })
    .attr("text-anchor", "middle")
    .text(d => label(d.data, d));
}

		if (title != null) cell.append("title")
				.text(d => title(d.data, d));

		return svg.node();
	}
</script>

<svelte:head>
	<link
	  rel="stylesheet"
	  href="https://unpkg.com/leaflet@1.6.0/dist/leaflet.css"
	  crossorigin=""
	/>
  
	<script
	  src="https://unpkg.com/leaflet@1.6.0/dist/leaflet.js"
	  crossorigin=""
	>
	</script>
  </svelte:head>
  
  
  <div id="map" style="float: left; width: 100%; height: 700px; position: absolute;">
	<div id="missing">
	  Nations with missing data <br> on location
	</div>
	<div id = "encoding">
		<h3> Visual encoding of bubble map:</h3>
		<ul>
		  <li>Bubble size: total revenue</li>
		  <li>Bubble color saturation: number of customer</li>
		  <li>Bubble outline color: the region that the nation belongs to</li>
		  </ul>
	  </div>
  <div id="chart" style="position: absolute; right: 50px; top: 20px; width: 25%; height: 1300px; z-index: 1000;">
  </div>
</div> 



<style>
#map {
    height: 660px;
}

#missing {
  position: absolute;
  top: 20px;
  left: 50px;
  font-weight: bold;
  font-size: 12px;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background: white;
  border-radius: 5px;
  padding: 5px 10px;
}

#encoding {
  position: absolute;
  right: 20px;
  bottom: 0px;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background: white;
  border-radius: 5px;
  padding: 5px 10px;
}

  h1 {
    color: #67000d;
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    text-align: center;
  }

  h3 {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  }

  ul {
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  }
</style>