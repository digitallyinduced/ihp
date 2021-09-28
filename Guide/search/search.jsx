import React from 'react';
import { DocSearch } from '@docsearch/react';
import ReactDOM from 'react-dom'

import '@docsearch/react/style';

function IHPSearch() {
  return <DocSearch apiKey="e2a57f1c68adf1b431ae8ee3cb0d3207" indexName="digitallyinduced" />
}

ReactDOM.render(
    <IHPSearch/>,
    document.getElementById('search')
);