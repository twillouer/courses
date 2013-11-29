/*
 * Copyright (c) 2008 - 2013 10gen, Inc. <http://10gen.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.tengen;

import com.mongodb.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.UnknownHostException;

public class Week2Homework2 {
    private static final Logger logger = LoggerFactory.getLogger("logger");

    public static void main(String[] args) throws UnknownHostException {
        MongoClient client = new MongoClient(new ServerAddress("localhost", 27017));

        DB database = client.getDB("students");
        final DBCollection collection = database.getCollection("grades");
        System.out.println(collection.count());

        QueryBuilder builder = new QueryBuilder().start("type").is("homework");
        DBCursor cursor = collection.find(builder.get()).sort(new BasicDBObject("student_id", 1).append("score", Integer.valueOf(1)));
        try {
            int oldStudentId = -1;
            while (cursor.hasNext()) {
                DBObject object = cursor.next();
                int newStudentId = (Integer) object.get("student_id");
                if (newStudentId != oldStudentId) {
                    System.out.println("on supprime " + object);
                    collection.remove(object);
                    oldStudentId = newStudentId;
                }
            }
        } finally {
            cursor.close();
        }

        System.out.println(collection.count());
    }
}
