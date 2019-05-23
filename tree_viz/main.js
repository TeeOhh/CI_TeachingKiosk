treeJson = d3.json('../scraping/acm-scraped-fields-small.json', function(error, treeData) {
    dTree.init(treeData, {
        target: '#graph',
        height: window.innerHeight || document.body.clientHeight,
        width: window.innerWidth || document.body.clientWidth,
        nodeWidth: 130
    });
});