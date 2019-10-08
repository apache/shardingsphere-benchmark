/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package perfstmt;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * sql statements used for performance.
 *
 * @author nancyzrh
 */
@RequiredArgsConstructor
@Getter
public enum ShardingPerfStmt {
    
    /**
     *  stmt for perf.
     */
    CLEAN_ALL_STMT("delete from ssperf"),
    
    DELETE_STMT("delete from ssperf where k=? and id=?"),
    
    INSERT_STMT("INSERT INTO ssperf(k,c,pad) VALUES (?,?,?)"),
    
    INSET_DEMO_STMT("INSERT INTO ssperf(id,k,c,pad) VALUES (?,?,?,?)"),
    
    SELECT_STMT("SELECT id,k from ssperf where id=1 and k=1"),
    
    UPDATE_STMT("update ssperf set c=?,pad=? where id=? and k=?");
    
    private final String value;
    
}
