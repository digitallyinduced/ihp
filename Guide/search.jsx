import React from 'react';
import { DocSearch } from '@docsearch/react';
import ReactDOM from 'react-dom'

import '@docsearch/react/style';

function IHPSearch() {
  return <DocSearch
    appId="JMAC7352QM"
    apiKey="097d3287e3009d4d3f666eefd7b05b1d"
    indexName="digitallyinduced"
  />
}

ReactDOM.render(
    <IHPSearch/>,
    document.getElementById('search')
);