import * as React from 'react';
import * as ReactDOM from 'react-dom';

import { DataSubscription, createRecord, updateRecord, deleteRecord, createRecords } from 'ihp-datasync/ihp-datasync';
import { query } from 'ihp-datasync/ihp-querybuilder';
import { useQuery, useGraphQLQuery } from 'ihp-datasync/react';
import * as GraphQL from 'ihp-datasync/graphql';

function HelloWorld() {
    // const tasks = useQuery(query('tasks'));
    const result = useGraphQLQuery("{ users { id email tasks { id title } } }");
    if (result === null) return <div>Loading</div>
    // const { task } = useGraphQLQuery('{ task(id: $id) { id, title } }', { id:  })
    return <div>
        {JSON.stringify(result)}

        <AddTask />
    </div>
}

function AddTask() {
    async function addTask(event) {
        event.preventDefault();
        const task = {
            title: 'Hello World',
            body: 'hello',
            userId: '40f1dbb4-403c-46fd-8062-fcf5362f2154'
        };

        const newTask = await GraphQL.query(`
            mutation {
                createTask(task: $task) { id }
            }
        `, { task });

        console.log('newTask', newTask);
    }

    return <button onClick={addTask}>Add Task</button>
}

function startApp() {
    ReactDOM.render(<HelloWorld/>, document.getElementById('hello-world'));
}

$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    startApp();
});
