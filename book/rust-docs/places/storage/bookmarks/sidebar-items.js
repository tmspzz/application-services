initSidebarItems({"constant":[["USER_CONTENT_ROOTS",""]],"enum":[["BookmarkPosition",""],["BookmarkRootGuid","Special GUIDs associated with bookmark roots. It’s guaranteed that the roots will always have these guids."],["BookmarkTreeNode",""],["FetchDepth",""],["InsertableItem",""],["UpdatableItem",""],["UpdateTreeLocation","Support for modifying bookmarks, including changing the location in the tree."]],"fn":[["bookmarks_get_url_for_keyword","Get the URL of the bookmark matching a keyword"],["create_bookmark_roots",""],["delete_bookmark","Delete the specified bookmark. Returns true if a bookmark with the guid existed and was deleted, false otherwise."],["delete_everything","Erases all bookmarks and resets all Sync metadata."],["fetch_tree","Fetch the tree starting at the specified guid. Returns a `BookmarkTreeNode`, its parent’s guid (if any), and position inside its parent."],["insert_bookmark",""],["insert_tree",""],["maybe_truncate_title",""],["update_bookmark",""]],"mod":[["bookmark_sync",""],["public_node",""]],"struct":[["BookmarkNode",""],["FolderNode",""],["InsertableBookmark","Structures which can be used to insert a bookmark, folder or separator."],["InsertableFolder",""],["InsertableSeparator",""],["SeparatorNode",""],["UpdatableBookmark","Structures which can be used to update a bookmark, folder or separator. Almost all fields are Option<>-like, with None meaning “do not change”. Many fields which can’t be changed by our public API are omitted (eg, guid, date_added, last_modified, etc)"],["UpdatableFolder",""],["UpdatableSeparator",""]]});