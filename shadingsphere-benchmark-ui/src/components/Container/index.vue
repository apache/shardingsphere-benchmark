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
  <div class="layout">
    <Layout>
      <Layout>
        <Header :style="{padding: 0}" class="layout-header-bar">
          <div class="i-layout-sider-logo i-layout-sider-logo-dark">
            <a href="/benchmark" target="_self" class="i-link i-link-color">
              <img class="logo" src="../../assets/logo_top.png" />
              <img class="collapse-logo" src="../../assets/logo.png" alt="logo" />
            </a>
          </div>
        </Header>
        <Content>
          <Breadcrumb class="bread-wrap">
            <BreadcrumbItem to="/">
              <Icon type="ios-home-outline" />&nbsp;Home
            </BreadcrumbItem>
            <BreadcrumbItem>{{ breadcrumbName }}</BreadcrumbItem>
          </Breadcrumb>
          <slot />
        </Content>
        <v-footer />
      </Layout>
    </Layout>
  </div>
</template>
<script>
import Vue from 'vue'
import { mapActions } from 'vuex'
import apis from '../../utils/page-apis'
import Footer from '../Footer/index.vue'

import { Layout, Header, Content, Breadcrumb, BreadcrumbItem, Icon } from 'iview'
Vue.component('Layout', Layout)
Vue.component('Header', Header)
Vue.component('Content', Content)
Vue.component('Breadcrumb', Breadcrumb)
Vue.component('BreadcrumbItem', BreadcrumbItem)
Vue.component('Icon', Icon)

export default {
  name: 'Container',
  components: {
    'v-footer': Footer
  },
  data() {
    return {
      isCollapsed: false,
      breadcrumbName: ''
    }
  },
  computed: {
    menuitemClasses() {
      return ['menu-item', this.isCollapsed ? 'collapsed-menu' : '']
    }
  },
  watch: {
    $route(to, from) {
      this.checkoutMenu(to.path)
      if (to.path === '/overview') {
        this.breadcrumbName = ''
      } else {
        const res = this.$store.state.global.fileData[
          to.path.split(`/overview_detail/`)[1]
        ]
        if (res) {
          this.breadcrumbName = res.split('-').join(' ')
        }
      }
    },
    '$store.state.global.fileData': {
      handler(val) {
        const res = val[location.hash.split(`#/overview_detail/`)[1]]
        if (res) {
          this.breadcrumbName = res.split('-').join(' ')
        }
      },
      deep: true
    }
  },
  mounted() {
    apis.getFileList().then(res => {
      const { fileName } = res
      this.setFileData(fileName)
    })
  },
  methods: {
    ...mapActions(['setMenu', 'setFileData']),
    checkoutMenu(path) {
      this.setMenu(path.split('/')[1])
    }
  }
}
</script>
<style scoped>
.bread-wrap {
  margin: 20px;
}
.layout {
  background: #f5f7f9;
  position: relative;
  overflow: hidden;
}
a {
  color: #fff;
}
.layout-header-bar {
  background: #fff;
  box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
}
.layout-logo-left {
  width: 90%;
  height: 30px;
  background: #5b6270;
  border-radius: 3px;
  margin: 15px auto;
}
.menu-icon {
  transition: all 0.3s;
}
.rotate-icon {
  transform: rotate(-90deg);
}
.menu-item {
  background: #1d1e23;
}
.menu-item span {
  display: inline-block;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  vertical-align: bottom;
  transition: width 0.2s ease 0.2s;
}
.menu-item i {
  transform: translateX(0px);
  transition: font-size 0.2s ease, transform 0.2s ease;
  vertical-align: middle;
  font-size: 16px;
}
.collapsed-menu span {
  width: 0px;
  transition: width 0.2s ease;
}
.collapsed-menu i {
  transform: translateX(5px);
  transition: font-size 0.2s ease 0.2s, transform 0.2s ease 0.2s;
  vertical-align: middle;
  font-size: 22px;
}
.i-layout-sider-logo {
  height: 63px;
  line-height: 63px;
  text-align: left;
  border-bottom: 1px solid #f8f8f9;
  margin-left: 10px;
}
.i-layout-sider-logo-dark {
  border-bottom: 1px solid #101117;
  background: #1d1e23;
}
.i-layout-sider-logo img {
  height: 74%;
  vertical-align: middle;
}
.layout-header-bar {
  background: #1d1e23;
  background: linear-gradient(90deg, #1d1e23, #3f4045);
}
.layout-sider {
  background: #1d1e23;
  z-index: 99;
  position: fixed;
  left: 0;
  height: 100%;
}
.layout-sider .ivu-layout-sider-children {
  background: #1d1e23 !important;
}
.ivu-layout-sider-collapsed .logo {
  display: none;
}
.ivu-layout-sider-collapsed .collapse-logo {
  display: inline-block;
}
.collapse-logo {
  display: none;
}
.ivu-menu-item-selected {
  background: #fff !important;
  color: #777 !important;
}
</style>
