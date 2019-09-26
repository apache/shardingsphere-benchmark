<template>
  <Row :gutter="16" style="background:#eee; padding:20px">
    <Col v-for="(value, name, index) in series" :key="index" class="col-item" span="24">
      <Card :bordered="false">
        <p slot="title" style="font-size: 18px">{{ name.toLowerCase() }} throughtout compare</p>
        <v-chart :options="getOptions(name)" />
        <Form :label-width="140">
          <FormItem
            v-for="(value, name,indx) in desc[name]"
            :label="`${name}:`"
            :key="indx"
          >{{ value }}</FormItem>
        </Form>
      </Card>
    </Col>
  </Row>
</template>

<script>
import ECharts from 'vue-echarts'
import 'echarts/lib/chart/line'
import 'echarts/lib/component/tooltip'
import 'echarts/lib/component/legend'
import 'echarts/lib/component/title'
import { shardingProxySingleDatabaseSingleTable } from '../../utils/utils'
import { mountedMixin } from '../../utils/mixin'
import { getLineOptions } from '../../utils/line'

export default {
  name: 'Home',
  components: {
    'v-chart': ECharts
  },
  mixins: [mountedMixin],
  data() {
    return {
      data: shardingProxySingleDatabaseSingleTable
    }
  },
  methods: {
    getOptions(name) {
      return getLineOptions(name, this.xAxis, this.legend, this.series)
    }
  }
}
</script>
<style>
.echarts {
  width: 100%;
  height: 400px;
}
.col-item {
  margin-bottom: 16px;
}
.ivu-form-item {
  margin-bottom: 10px !important;
}
.ivu-form-item-label {
  font-size: 15px !important;
}
.ivu-form-item-content {
  font-size: 14px !important;
}
</style>