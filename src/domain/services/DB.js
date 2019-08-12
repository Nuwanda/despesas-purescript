exports._getAll = function(dict) {
  return function(index) {
    return function(reject, resolve) {
      const request = index.getAll();

      request.onsuccess = evt => {
        const values = evt.target.result;
        const exps = values.map(a => a);
        window.exps = exps;
        resolve(exps);
      };
      request.onerror = evt =>
        resolve('Error fetching objects: ' + evt.target.errorCode);
    };
  };
};

exports._download = function() {
  exports._getAll(data => {
    const json = JSON.stringify(data);
    const uri = encodeURIComponent(json);
    const node = document.createElement('a');
    node.setAttribute('href', 'data:text/plain;charset=utf-8,' + uri);
    node.setAttribute('download', 'expensesDB.txt');
    node.setAttribute('style', 'display: none');
    document.body.appendChild(node);
    node.click();
    node.parentNode.removeChild(node);
  });
};
