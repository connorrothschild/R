d3
	.csv('../data/raw_data.csv', (d) => ({
		id           : d.id,
		diverse_1994 : d.diverse_1994,
		diverse_2016 : d.diverse_2016,
		total_1994   : d.total_1994,
		total_2016   : d.total_2016
	}))
	.then((data) => {
		const width = 1200;
		const height = 1200;

		const svg = d3.select('svg').attr('width', width).attr('height', height);

		const xCenter = [ width / 4, 3 * width / 4 ];
		const yCenter = [ height / 4, height / 2, 3 * height / 4 ];
		const colors = [ '#d5ece6', '#9ccadd', '#b394c9' ];
		const factor = 10;
		const delay = factor * 200;
		const animDuration = factor * 1000;
		const tolerance = 0.6;

		const maxTotal = d3.max(data.map((d) => +d.total_1994).concat(data.map((d) => +d.total_2016)));

		const totalScale = d3.scaleSqrt().domain([ 0, maxTotal ]).range([ 1, 20 ]);

		data = data.map(function(d) {
			const sourceIndex = +d.diverse_1994;
			const targetIndex = +d.diverse_2016;

			return {
				id           : d.id,
				x            : +xCenter[0] + 200 * Math.random(),
				y            : +yCenter[sourceIndex] + 200 * Math.random(),
				sourceRadius : 10,
				targetRadius : 10,
				color        : colors[sourceIndex],
				sourceX      : +xCenter[0] + 200 * Math.random(),
				targetX      : +xCenter[1] + 200 * Math.random(),
				sourceY      : +yCenter[sourceIndex] + 200 * Math.random(),
				targetY      : +yCenter[targetIndex] + 200 * Math.random(),
				random       : Math.random()
			};
		});

		console.log(data);

		svg
			.selectAll('.gray_circle')
			.data(data)
			.enter()
			.append('circle')
			.attr('class', 'gray_circle')
			.attr('r', (d) => d.sourceRadius)
			.attr('fill', '#efefef')
			.attr('cx', (d) => d.x)
			.attr('cy', (d) => d.y);

		svg
			.selectAll('.circle')
			.data(data)
			.enter()
			.append('circle')
			.attr('class', 'circle')
			.attr('r', (d) => d.sourceRadius)
			.attr('fill', (d) => d.color)
			.attr('cx', (d) => d.x)
			.attr('cy', (d) => d.y)
			.attr('opacity', 0.3);

		svg
			.selectAll('.years_label')
			.data([ { label: '2016', x: xCenter[0], y: 100 }, { label: '2020', x: xCenter[1], y: 100 } ])
			.enter()
			.append('text')
			.attr('class', 'years_label')
			.attr('x', (d) => d.x)
			.attr('y', (d) => d.y)
			.text((d) => d.label);

		svg
			.selectAll('.diversity_label')
			.data([
				{ label: 'Voted for Sanders', x: xCenter[0], y: yCenter[0] },
				{ label: 'Voted for Clinton', x: xCenter[0], y: yCenter[1] }
				// { label: 'Diverse', x: xCenter[0], y: yCenter[2] }
			])
			.enter()
			.append('text')
			.attr('class', 'diversity_label')
			.attr('x', (d) => d.x)
			.attr('y', (d) => d.y)
			.text((d) => d.label);

		svg
			.selectAll('.description_label')
			.data([
				{ label: '261 counties (26%)', x: xCenter[0], y: yCenter[0] + 30 },
				{ label: '729 counties (74%)', x: xCenter[0], y: yCenter[1] + 30 }
				// { label: 'Less than 75%', x: xCenter[0], y: yCenter[2] + 30 }
			])
			.enter()
			.append('text')
			.attr('class', 'description_label')
			.attr('x', (d) => d.x)
			.attr('y', (d) => d.y)
			.text((d) => d.label);

		svg
			.selectAll('.diversity_label')
			.transition()
			.ease(d3.easePoly)
			.duration((delay + animDuration) / 8.0)
			.delay(7 * (delay + animDuration) / 8.0)
			.attr('x', (d) => d3.mean(xCenter));

		svg
			.selectAll('.description_label')
			// .exit()
			// .remove()
			// .enter()
			// .data([
			// 	{ label: 'TESTING counties (26%)', x: xCenter[0], y: yCenter[0] + 30 },
			// 	{ label: 'TESTING counties (74%)', x: xCenter[0], y: yCenter[1] + 30 }
			// ])
			.transition()
			.ease(d3.easePoly)
			.duration((delay + animDuration) / 8.0)
			.delay(7 * (delay + animDuration) / 8.0)
			.attr('x', (d) => d3.mean(xCenter));

		svg
			.selectAll('.circle')
			.transition()
			.ease(d3.easePoly.exponent(5))
			.duration((d) => Math.floor((tolerance * (d.random - 1) + 1) * animDuration))
			.delay((d) => Math.floor(delay + (1 - d.random) * tolerance * animDuration))
			.attr('r', (d) => d.targetRadius)
			.attr('cx', (d) => d.targetX)
			.attr('cy', (d) => d.targetY)
			.attr('opacity', 0.3);

		svg
			.selectAll('.gray_circle')
			.transition()
			.ease(d3.easePoly.exponent(5))
			.duration(Math.floor((1 - tolerance) * animDuration))
			.delay(Math.floor(delay + tolerance * animDuration))
			.attr('fill', (d) => d.color)
			.attr('opacity', 0.3);

		const strength = 0.01;

		function ticked() {
			d3
				.selectAll('.circle')
				.attr('class', 'circle')
				.attr('r', (d) => d.sourceRadius)
				.attr('fill', (d) => d.color)
				.attr('cx', (d) => d.x)
				.attr('cy', (d) => d.y);
		}

		const simulation = d3
			.forceSimulation(data)
			.force('x', d3.forceX().x((d) => d.sourceX).strength(strength))
			.force('y', d3.forceY().y((d) => d.sourceY).strength(strength))
			.alpha(1)
			.on('tick', ticked);

		// function updateNodes() {
		// 	setTimeout((d) => console.log(d3.csvFormat(data)), 10000);

		simulation
			.alphaDecay(0.01)
			.force('x', d3.forceX().x((d) => d.targetX).strength(strength))
			.force('y', d3.forceY().y((d) => d.targetY).strength(strength))
			.restart();

		// setTimeout(updateNodes, 1000);
	});
