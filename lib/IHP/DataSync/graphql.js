import { DataSyncController  } from "./ihp-datasync";

export async function query(gql, variables = {}) {
    if (typeof gql !== "string") {
        throw new Error(`Query needs to be a string, you passed ${JSON.stringify(gql)} in a call to query(${JSON.stringify(gql)}, ${JSON.stringify(variables)})`);
    }
    if (variables !== Object(variables)) {
        throw new Error(`Variables needs to be an object, you passed ${JSON.stringify(variables)} in a call to query(${JSON.stringify(gql)}, ${JSON.stringify(variables, null, 4)})`);
    }
    const { graphQLResult } = await DataSyncController.getInstance().sendMessage({ tag: 'GraphQLRequest', gql, variables });
    return graphQLResult;
}