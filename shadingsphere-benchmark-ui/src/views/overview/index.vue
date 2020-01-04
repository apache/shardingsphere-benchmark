<!--
  - Licensed to the Apache Software Foundation (ASF) under one or more
  - contributor license agreements.  See the NOTICE file distributed with
  - this work for additional information regarding copyright ownership.
  - The ASF licenses this file to You under the Apache License, Version 2.0
  - (the "License"); you may not use this file except in compliance with
  - the License.  You may obtain a copy of the License at
  -
  -     http://www.apache.org/licenses/LICENSE-2.0
  -
  - Unless required by applicable law or agreed to in writing, software
  - distributed under the License is distributed on an "AS IS" BASIS,
  - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  - See the License for the specific language governing permissions and
  - limitations under the License.
  -->
<template>
  <Card class="card-wrap">
    <p slot="title">
      <Icon type="ios-analytics" />&nbsp;Overview
    </p>
    <Row :gutter="16">
      <Col
        v-for="(item, index) in $store.state.global.fileData"
        :key="index"
        :xs="24"
        :sm="12"
        :md="12"
        :lg="6"
        :xl="6"
      >
        <Card class="card-item">
          <router-link :to="'/overview_detail/' + index">
            <div :class="'card-item-left left0' + index">0{{ index + 1 }}</div>
            <h3>{{ item.split('_').join(' ') }}</h3>
            <span class="card-span">detail</span>
          </router-link>
        </Card>
      </Col>
    </Row>
  </Card>
</template>
<script>
import Vue from 'vue'
import { Card, Row, Col, Icon } from 'iview'
Vue.component('Card', Card)
Vue.component('Row', Row)
Vue.component('Col', Col)
Vue.component('Icon', Icon)
export default {
  name: 'Overview'
}
</script>
<style scoped>
.card-wrap {
  margin: 20px;
}
.card-item {
  min-height: 150px;
  position: relative;
  margin-bottom: 15px;
}
h3 {
  color: #333;
}
.card-span {
  position: absolute;
  right: 10px;
  bottom: 10px;
}
.card-item-left {
  width: 50px;
  height: 50px;
  float: left;
  line-height: 50px;
  text-align: center;
  border-radius: 50px;
  margin-right: 10px;
  font-size: 18px;
  font-weight: bold;
  color: #fff;
}
.left00 {
  background: #1ba3f7;
}
.left01 {
  background: #d08553;
}
.left02 {
  background: #fd9927;
}
.left03 {
  background: #a177ca;
}
.left04 {
  background: #00b0cf;
}
.left05 {
  background: #8fa6ee;
}
</style>