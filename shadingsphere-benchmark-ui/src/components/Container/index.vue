<template>
  <div class="layout">
    <Layout>
      <Sider
        ref="side1"
        v-model="isCollapsed"
        :collapsed-width="78"
        class="layout-sider"
        hide-trigger
        collapsible
        width="300"
      >
        <div class="i-layout-sider-logo i-layout-sider-logo-dark">
          <a href="/" target="_self" class="i-link i-link-color">
            <img class="logo" src="../../assets/logo_top.png" />
            <img class="collapse-logo" src="../../assets/logo.png" alt="logo" />
          </a>
        </div>
        <Menu :class="menuitemClasses" active-name="1-2" theme="dark" width="auto">
          <router-link to="mysql-vs-sharding">
            <MenuItem name="1-2">
              <span>mysqlVsSharding</span>
            </MenuItem>
          </router-link>
          <router-link to="sharding-proxy-master-slave">
            <MenuItem name="1-3">
              <span>shardingProxyMasterSlave</span>
            </MenuItem>
          </router-link>
          <router-link to="sharding-proxy-master-slave-sharding">
            <MenuItem name="1-4">
              <span>shardingProxyMasterSlaveSharding</span>
            </MenuItem>
          </router-link>
          <router-link to="sharding-proxy-single-database-single-table">
            <MenuItem name="1-5">
              <span>shardingProxySingleDatabaseSingleTable</span>
            </MenuItem>
          </router-link>
        </Menu>
      </Sider>
      <Layout style="margin-left: 300px;">
        <Header :style="{padding: 0}" class="layout-header-bar">
          <!-- <Icon
            :class="rotateIcon"
            :style="{margin: '0 20px', color: '#f6ca9d', cursor: 'pointer'}"
            size="24"
            type="md-menu"
            @click.native="collapsedSider"
          />-->
        </Header>
        <Content>
          <slot />
        </Content>
        <v-footer />
      </Layout>
    </Layout>
  </div>
</template>
<script>
import Footer from '../Footer/index.vue'
export default {
  name: 'Container',
  components: {
    'v-footer': Footer
  },
  data() {
    return {
      isCollapsed: false
    }
  },
  computed: {
    rotateIcon() {
      return ['menu-icon', this.isCollapsed ? 'rotate-icon' : '']
    },
    menuitemClasses() {
      return ['menu-item', this.isCollapsed ? 'collapsed-menu' : '']
    }
  },
  methods: {
    collapsedSider() {
      // this.$refs.side1.toggleCollapse()
    }
  }
}
</script>
<style scoped>
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
  text-align: center;
  border-bottom: 1px solid #f8f8f9;
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
