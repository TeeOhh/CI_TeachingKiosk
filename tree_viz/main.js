treeJson = d3.json('../scraping/acm_scraped_fields_small.json', function(error, treeData) {
    dTree.init(treeData, {
        target: '#graph',
        height: window.innerHeight || document.body.clientHeight,
        width: window.innerWidth || document.body.clientWidth,
        nodeWidth: 130
    });
});