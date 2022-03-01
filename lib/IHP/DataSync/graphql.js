import { DataSyncController  } from "./ihp-datasync.js";

export function query(gql) {
    return DataSyncController.getInstance().sendMessage({ tag: 'GraphQLRequest', gql });
}